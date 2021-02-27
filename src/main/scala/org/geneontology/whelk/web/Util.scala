package org.geneontology.whelk.web

import org.geneontology.archimedes.owl.{AnnotationAssertion, AnnotationProperty, Class, EquivalentClasses, IRI, Literal, Ontology, PlainLiteral, SubClassOf, TypedLiteral}
import org.geneontology.whelk.archimedes.Bridge
import org.geneontology.whelk.{AtomicConcept, BuiltIn, Reasoner, ReasonerState}

object Util {

  val RDFSLabel: AnnotationProperty = AnnotationProperty(IRI("http://www.w3.org/2000/01/rdf-schema#label"))

  final case class Inferences(
                               ontology: Ontology,
                               reasonerState: ReasonerState,
                               taxonomy: Map[AtomicConcept, (Set[AtomicConcept], Set[AtomicConcept])],
                               time: Long
                             )

  final case class LabeledClass(cls: Class, label: String)

  final case class Relation(label: String, term1: LabeledClass, term2: LabeledClass)

  def computeInferences(ontology: Ontology): Inferences = {
    val start = System.currentTimeMillis()
    val axioms = Bridge.ontologyToAxioms(ontology)
    val whelk = Reasoner.assert(axioms)
    val taxonomy = whelk.computeTaxonomy
    val stop = System.currentTimeMillis()
    val time = stop - start
    Inferences(ontology, whelk, taxonomy, time)
  }

  def assertedSubClassOfIndex(ontology: Ontology): Map[Class, Set[Class]] = ontology.axioms.foldLeft(Map.empty[Class, Set[Class]]) {
    case (acc, SubClassOf(sub @ Class(_), sup @ Class(_), _)) =>
      acc.updated(sub, acc.getOrElse(sub, Set.empty) + sup)
    case (acc, _)                                             => acc
  }

  def assertedEquivalentsIndex(ontology: Ontology): Map[Class, Set[Class]] = ontology.axioms.foldLeft(Map.empty[Class, Set[Class]]) {
    case (acc, EquivalentClasses(classes, _)) =>
      classes.items.to(List).combinations(2).foldLeft(acc) {
        case (acc2, (first @ Class(_)) :: (second @ Class(_)) :: Nil) =>
          acc2.updated(first, acc.getOrElse(first, Set.empty) + second)
            .updated(second, acc.getOrElse(second, Set.empty) + first)
        case (acc2, _)                                                => acc2
      }
    case (acc, _)                             => acc
  }

  def inferredSubsumptions(inferences: Inferences, labels: Map[IRI, String]): List[Relation] = {
    val subClassOfWithoutAnnotations = Util.assertedSubClassOfIndex(inferences.ontology)
    val equivToWithoutAnnotations = Util.assertedEquivalentsIndex(inferences.ontology)
    inferences.taxonomy.to(List)
      .filterNot(_._1 == BuiltIn.Bottom)
      .flatMap { case (AtomicConcept(subclass), (equivs, superclasses)) =>
        val sub = Class(IRI(subclass))
        val subLabel = labels.getOrElse(sub.iri, s"<$subclass>")
        val equivRels = equivs.to(List)
          .map(c => Class(IRI(c.id)))
          .filterNot(c => equivToWithoutAnnotations.getOrElse(sub, Set.empty)(c))
          .map { equiv =>
            val equivLabel = labels.getOrElse(equiv.iri, s"<${equiv.iri.id}>")
            Relation("EquivalentTo", LabeledClass(sub, subLabel), LabeledClass(equiv, equivLabel))
          }
        val superRels = superclasses.to(List)
          .filterNot(_ == BuiltIn.Top)
          .map(c => Class(IRI(c.id)))
          .filterNot(c => subClassOfWithoutAnnotations.getOrElse(sub, Set.empty)(c))
          .map { superclass =>
            val superclassLabel = labels.getOrElse(superclass.iri, s"<${superclass.iri.id}>")
            Relation("SubClassOf", LabeledClass(sub, subLabel), LabeledClass(superclass, superclassLabel))
          }
        equivRels ::: superRels
      }
  }

  def labelIndex(ontology: Ontology): Map[IRI, String] =
    ontology.axioms.collect { case AnnotationAssertion(RDFSLabel, iri: IRI, literal: Literal, _) =>
      iri -> lexicalForm(literal)
    }.to(Map)

  private def lexicalForm(literal: Literal): String = literal match {
    case PlainLiteral(text, _) => text
    case TypedLiteral(text, _) => text
  }

}
