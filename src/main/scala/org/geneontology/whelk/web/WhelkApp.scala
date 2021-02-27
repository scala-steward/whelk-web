package org.geneontology.whelk.web

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.geneontology.archimedes.io.OWLFunctionalSyntaxReader
import org.geneontology.archimedes.owl._
import org.geneontology.whelk.web.Util.{Inferences, computeInferences}
import org.scalajs.dom
import org.scalajs.dom.html.Paragraph

object WhelkApp {

  val ontologyTextBus: EventBus[String] = new EventBus[String]()

  val $ontology: Signal[Either[String, Ontology]] =
    ontologyTextBus.events.debounce(300).map { text =>
      if (text.isEmpty) Left("No ontology provided")
      else OWLFunctionalSyntaxReader.readOntology(text)
    }.toSignal(Left("No ontology provided"))

  val $ontologyReport: Signal[ReactiveHtmlElement[Paragraph]] = $ontology.map {
    case Right(ontology) =>
      val annotationAxioms = ontology.axioms.count(_.isInstanceOf[AnnotationAxiom])
      val logicalAxioms = ontology.axioms.count(_.isInstanceOf[LogicalAxiom])
      p(
        s"Ontology: ${ontology.id.map(id => s"<${id.iri.id}>").getOrElse("<anonymous>")}",
        ul(
          li(s"$annotationAxioms annotation axioms"),
          li(s"$logicalAxioms logical axioms"),
        )
      )
    case Left(error)     => p(s"Ontology parse error: $error")
  }

  val $labelIndex: Signal[Map[IRI, String]] = $ontology.map(_.map(Util.labelIndex).getOrElse(Map.empty))

  val $inferences: Signal[Option[Inferences]] = $ontology.map(_.toOption.map(computeInferences))

  val $renderedSubsumptions: Signal[List[Li]] = $inferences.combineWith($labelIndex).map {
    case (Some(inferences), labels) =>
      Util.inferredSubsumptions(inferences, labels)
        .sortBy(rel => s"${rel.term1.label}${rel.term2.label}")
        .map { rel =>
          li(
            a(href := rel.term1.cls.iri.id, rel.term1.label),
            " ",
            i(rel.label),
            " ",
            a(href := rel.term2.cls.iri.id, rel.term2.label)
          )
        }
    case _                          => Nil
  }

  val $reasoningTimeMessage: Signal[Option[ReactiveHtmlElement[Paragraph]]] = $inferences.map { maybeInferences =>
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
          child <-- $ontologyReport
        ),
        div(
          cls := "w3-container w3-half",
          h3("Inferred subsumptions:"),
          p(child.maybe <-- $reasoningTimeMessage),
          ul(
            children <-- $renderedSubsumptions
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
