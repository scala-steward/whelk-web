package org.geneontology.whelk.web

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.geneontology.archimedes.io.OWLFunctionalSyntaxReader
import org.geneontology.archimedes.owl._
import org.geneontology.whelk.web.Util.{Inferences, Relation, computeInferences, iri}
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

  val $inferencesAndLabels = $inferences.combineWith($labelIndex)

  val $renderedSubsumptions: Signal[List[Li]] = $inferencesAndLabels.map {
    case (Some(inferences), labels) =>
      renderRelations(Util.inferredSubsumptions(inferences, labels))
    case _                          => Nil
  }

  val $renderedPropertyAssertions: Signal[List[Li]] = $inferencesAndLabels.map {
    case (Some(inferences), labels) => renderRelations(Util.inferredPropertyAssertions(inferences, labels))
    case _                          => Nil
  }

  val $renderedClassAssertions: Signal[List[Li]] = $inferencesAndLabels.map {
    case (Some(inferences), labels) => renderRelations(Util.inferredClassAssertions(inferences, labels))
    case _                          => Nil
  }

  def renderRelations(relations: List[Relation]): List[Li] =
    relations.sortBy(rel => s"${rel.term1.label}${rel.term2.label}")
      .map { rel =>
        li(
          a(href := iri(rel.term1.cls).id, rel.term1.label),
          " ",
          i(rel.label),
          " ",
          a(href := iri(rel.term2.cls).id, rel.term2.label)
        )
      }

  val $reasoningTimeMessage: Signal[Option[ReactiveHtmlElement[Paragraph]]] = $inferences.map { maybeInferences =>
    maybeInferences.map(inf => p(s"Reasoning done in: ${inf.time}ms"))
  }

  val appDiv: Div =
    div(
      cls := "w3-container",
      div(
        cls := "w3-row-padding",
        h1(
          styleAttr := "margin-top: 0",
          "Whelk on the Web"),
        p(
          a(href := "https://github.com/balhoff/whelk", "Whelk"),
          " is an OWL EL+RL reasoner written in ",
          a(href := "https://www.scala-lang.org", "Scala"),
          ". Whelk works best on the JVM, but here it is running directly in a browser, using ",
          a(href := "https://www.scala-js.org", "Scala.js"), "."),
      ),
      div(
        cls := "w3-row-padding",
        div(
          cls := "w3-container w3-half",
          h3("Ontology in OWL functional syntax (imports will not be followed):"),
          textArea(
            styleAttr := "font-family: monospace; font-size: small; line-height: normal",
            width := "100%",
            height := "20em",
            controlled(
              value <-- ontologyTextBus.events,
              onInput.mapToValue --> ontologyTextBus
            )
          ),
          button("Try families ontology example",
            onClick.mapTo(FamiliesOntology.text) --> ontologyTextBus
          ),
          child <-- $ontologyReport
        ),
        div(
          cls := "w3-container w3-half",
          h3("Reasoning results"),
          p(child.maybe <-- $reasoningTimeMessage),
          h4("Inferred subsumptions:"),
          ul(styleAttr := "font-size: small; line-height: normal",
            children <-- $renderedSubsumptions
          ),
          h4("Inferred class assertions:"),
          ul(styleAttr := "font-size: small; line-height: normal",
            children <-- $renderedClassAssertions
          ),
          h4("Inferred property assertions:"),
          ul(styleAttr := "font-size: small; line-height: normal",
            children <-- $renderedPropertyAssertions
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
