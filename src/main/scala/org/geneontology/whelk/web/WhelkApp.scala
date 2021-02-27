package org.geneontology.whelk.web

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.geneontology.archimedes.io.OWLFunctionalSyntaxReader
import org.geneontology.archimedes.owl._
import org.geneontology.whelk.archimedes.Bridge
import org.geneontology.whelk.{AtomicConcept, BuiltIn, Reasoner, ReasonerState}
import org.scalajs.dom
import org.scalajs.dom.html.Paragraph

object WhelkApp {

  val RDFSLabel: AnnotationProperty = AnnotationProperty(IRI("http://www.w3.org/2000/01/rdf-schema#label"))

  final case class Inferences(
                               ontology: Ontology,
                               reasonerState: ReasonerState,
                               taxonomy: Map[AtomicConcept, (Set[AtomicConcept], Set[AtomicConcept])],
                               time: Long
                             )

  private def lexicalForm(literal: Literal): String = literal match {
    case PlainLiteral(text, _) => text
    case TypedLiteral(text, _) => text
  }

  val ontologyTextBus: EventBus[String] = new EventBus[String]()

  val ontologyStream: EventStream[Either[String, Ontology]] =
    ontologyTextBus.events.debounce(300).map { text =>
      if (text.isEmpty) Left("No ontology provided")
      else OWLFunctionalSyntaxReader.readOntology(text)
    }

  val ontologyReportStream: EventStream[ReactiveHtmlElement[Paragraph]] = ontologyStream.map {
    case Right(ontology) =>
      val annotationAxioms = ontology.axioms.count(_.isInstanceOf[AnnotationAxiom])
      val logicalAxioms = ontology.axioms.count(_.isInstanceOf[LogicalAxiom])
      p(
        s"Ontology: ${ontology.id.map(_.iri.id).map(id => s"<$id>").getOrElse("<anonymous>")}",
        ul(
          li(s"$annotationAxioms annotation axioms"),
          li(s"$logicalAxioms logical axioms"),
        )
      )
    case Left(error)     => p(s"Ontology parse error: $error")
  }

  val labelIndexStream: EventStream[Map[IRI, String]] = ontologyStream.map { maybeOntology =>
    maybeOntology.map { ontology =>
      ontology.axioms.collect { case AnnotationAssertion(RDFSLabel, iri: IRI, literal: Literal, _) =>
        iri -> lexicalForm(literal)
      }.to(Map)
    }.getOrElse(Map.empty)
  }

  val inferencesStream: EventStream[Option[Inferences]] = ontologyStream.map(_.toOption).map { maybeOntology =>
    maybeOntology.map { ontology =>
      val start = System.currentTimeMillis()
      val axioms = Bridge.ontologyToAxioms(ontology)
      val whelk = Reasoner.assert(axioms)
      val taxonomy = whelk.computeTaxonomy
      val stop = System.currentTimeMillis()
      val time = stop - start
      Inferences(ontology, whelk, taxonomy, time)
    }
  }

  def assertedSubClassOfIndex(ontology: Ontology): Map[IRI, Set[IRI]] = ontology.axioms.foldLeft(Map.empty[IRI, Set[IRI]]) {
    case (acc, SubClassOf(Class(sub), Class(sup), _)) =>
      acc.updated(sub, acc.getOrElse(sub, Set.empty) + sup)
    case (acc, _)                                     => acc
  }

  def assertedEquivalentsIndex(ontology: Ontology): Map[IRI, Set[IRI]] = ontology.axioms.foldLeft(Map.empty[IRI, Set[IRI]]) {
    case (acc, EquivalentClasses(classes, _)) =>
      classes.items.to(List).combinations(2).foldLeft(acc) {
        case (acc2, Class(first) :: Class(second) :: Nil) =>
          acc2.updated(first, acc.getOrElse(first, Set.empty) + second)
            .updated(second, acc.getOrElse(second, Set.empty) + first)
        case (acc2, _)                                    => acc2
      }
    case (acc, _)                             => acc
  }

  val renderedSubsumptions: EventStream[List[Li]] = inferencesStream.combineWith(labelIndexStream).map {
    case (Some(inferences), labels) =>
      val subClassOfWithoutAnnotations = assertedSubClassOfIndex(inferences.ontology)
      val equivToWithoutAnnotations = assertedEquivalentsIndex(inferences.ontology)
      inferences.taxonomy.to(List)
        .filterNot(_._1 == BuiltIn.Bottom)
        .flatMap { case (AtomicConcept(subclass), (equivs, superclasses)) =>
          val sub = IRI(subclass)
          val subLabel = labels.getOrElse(sub, s"<$subclass>")
          val equivLis = equivs.to(List)
            .map(c => IRI(c.id))
            .filterNot(c => equivToWithoutAnnotations.getOrElse(sub, Set.empty)(c))
            .map { equiv =>
              val equivLabel = labels.getOrElse(equiv, s"<${equiv.id}>")
              li(
                a(href := subclass, subLabel),
                i(" EquivalentTo "),
                a(href := equiv.id, equivLabel)
              )
            }
          val superLis = superclasses.to(List)
            .filterNot(_ == BuiltIn.Top)
            .map(c => IRI(c.id))
            .filterNot(c => subClassOfWithoutAnnotations.getOrElse(sub, Set.empty)(c))
            .map { superclass =>
              val superclassLabel = labels.getOrElse(superclass, s"<${superclass.id}>")
              li(
                a(href := subclass, subLabel),
                i(" SubClassOf "),
                a(href := superclass.id, superclassLabel)
              )
            }
          equivLis ::: superLis
        }
    case _                          => Nil
  }

  val reasoningTimeMessage: EventStream[Option[ReactiveHtmlElement[Paragraph]]] = inferencesStream.map { maybeInferences =>
    maybeInferences.map(inf => p(s"Reasoning done in: ${inf.time}ms"))
  }


  val appDiv: Div =
    div(
      cls := "w3-container",
      div(
        cls := "w3-row-padding",
        h1("Whelk on the Web")
      ),
      div(
        cls := "w3-row-padding",
        div(
          cls := "w3-container w3-half",
          h3("Ontology in OWL functional syntax (imports will not be followed):"),
          textArea(
            width := "100%",
            height := "20em",
            inContext(thisNode => onInput.mapTo(thisNode.ref.value) --> ontologyTextBus)
          ),
          child <-- ontologyReportStream
        ),
        div(
          cls := "w3-container w3-half",
          h3("Inferred subsumptions:"),
          p(child.maybe <-- reasoningTimeMessage),
          ul(
            children <-- renderedSubsumptions
          )
        )
      )
    )

  def main(args: Array[String]): Unit = {
    documentEvents.onDomContentLoaded.foreach {
      _ =>
        val appContainer = dom.document.querySelector("#appContainer")
        render(appContainer, appDiv)
    }(unsafeWindowOwner)
  }

}
