package org.geneontology.whelk.web

import org.geneontology.archimedes.owl.{AnnotationAssertion, AnnotationProperty, Class, ClassAssertion, Entity, EquivalentClasses, IRI, Literal, NamedIndividual, ObjectProperty, ObjectPropertyAssertion, Ontology, PlainLiteral, SubClassOf, TypedLiteral}
import org.geneontology.whelk.archimedes.Bridge
import org.geneontology.whelk.{AtomicConcept, BuiltIn, Individual, Reasoner, ReasonerState, Role, RoleAssertion}

object Util {

  val RDFSLabel: AnnotationProperty = AnnotationProperty(IRI("http://www.w3.org/2000/01/rdf-schema#label"))

  final case class Inferences(
                               ontology: Ontology,
                               reasonerState: ReasonerState,
                               taxonomy: Map[AtomicConcept, (Set[AtomicConcept], Set[AtomicConcept])],
                               time: Long
                             )

  final case class LabeledEntity(cls: Entity, label: String)

  final case class Relation(label: String, term1: LabeledEntity, term2: LabeledEntity)

  def computeInferences(ontology: Ontology): Inferences = {
    val start = System.currentTimeMillis()
    val axioms = Bridge.ontologyToAxioms(ontology)
    val whelk = Reasoner.assert(axioms)
    val taxonomy = whelk.computeTaxonomy
    val stop = System.currentTimeMillis()
    val time = stop - start
    Inferences(ontology, whelk, taxonomy, time)
  }

  def assertedSubClassOfIndex(ontology: Ontology): Map[Class, Set[Class]] =
    ontology.axioms.foldLeft(Map.empty[Class, Set[Class]]) {
      case (acc, SubClassOf(sub @ Class(_), sup @ Class(_), _)) =>
        acc.updated(sub, acc.getOrElse(sub, Set.empty) + sup)
      case (acc, _)                                             => acc
    }

  def assertedEquivalentsIndex(ontology: Ontology): Map[Class, Set[Class]] =
    ontology.axioms.foldLeft(Map.empty[Class, Set[Class]]) {
      case (acc, EquivalentClasses(classes, _)) =>
        classes.items.to(List).combinations(2).foldLeft(acc) {
          case (acc2, (first @ Class(_)) :: (second @ Class(_)) :: Nil) =>
            acc2.updated(first, acc.getOrElse(first, Set.empty) + second)
              .updated(second, acc.getOrElse(second, Set.empty) + first)
          case (acc2, _)                                                => acc2
        }
      case (acc, _)                             => acc
    }

  def assertedPropertyAssertionsIndex(ontology: Ontology): Map[NamedIndividual, Map[ObjectProperty, Set[NamedIndividual]]] =
    ontology.axioms.foldLeft(Map.empty[NamedIndividual, Map[ObjectProperty, Set[NamedIndividual]]]) {
      case (acc, ObjectPropertyAssertion(prop: ObjectProperty, subject: NamedIndividual, target: NamedIndividual, _)) =>
        val currentPropMap = acc.getOrElse(subject, Map.empty)
        val currentTargets = currentPropMap.getOrElse(prop, Set.empty)
        acc.updated(subject, currentPropMap.updated(prop, currentTargets + target))
      case (acc, _)                                                                                                   => acc
    }

  def assertedClassAssertionsIndex(ontology: Ontology): Map[NamedIndividual, Set[Class]] =
    ontology.axioms.foldLeft(Map.empty[NamedIndividual, Set[Class]]) {
      case (acc, ClassAssertion(cls: Class, individual: NamedIndividual, _)) =>
        acc.updated(individual, acc.getOrElse(individual, Set.empty) + cls)
      case (acc, _)                                                          => acc
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
            Relation("EquivalentTo", LabeledEntity(sub, subLabel), LabeledEntity(equiv, equivLabel))
          }
        val superRels = superclasses.to(List)
          .filterNot(_ == BuiltIn.Top)
          .map(c => Class(IRI(c.id)))
          .filterNot(c => subClassOfWithoutAnnotations.getOrElse(sub, Set.empty)(c))
          .map { superclass =>
            val superclassLabel = labels.getOrElse(superclass.iri, s"<${superclass.iri.id}>")
            Relation("SubClassOf", LabeledEntity(sub, subLabel), LabeledEntity(superclass, superclassLabel))
          }
        equivRels ::: superRels
      }
  }

  def inferredPropertyAssertions(inferences: Inferences, labels: Map[IRI, String]): List[Relation] = {
    val asserted = assertedPropertyAssertionsIndex(inferences.ontology)
    inferences.reasonerState.roleAssertions.to(List)
      .filterNot { case RoleAssertion(Role(roleID), Individual(subjectID), Individual(targetID)) =>
        asserted.getOrElse(NamedIndividual(IRI(subjectID)), Map.empty)
          .getOrElse(ObjectProperty(IRI(roleID)), Set.empty)(NamedIndividual(IRI(targetID)))
      }
      .map { case RoleAssertion(Role(roleID), Individual(subjectID), Individual(targetID)) =>
        val propIRI = IRI(roleID)
        val subjectIRI = IRI(subjectID)
        val targetIRI = IRI(targetID)
        val propLabel = labels.getOrElse(propIRI, propIRI.id)
        val subjectLabel = labels.getOrElse(subjectIRI, subjectIRI.id)
        val targetLabel = labels.getOrElse(targetIRI, targetIRI.id)
        Relation(propLabel,
          LabeledEntity(NamedIndividual(subjectIRI), subjectLabel),
          LabeledEntity(NamedIndividual(targetIRI), targetLabel))
      }
  }

  def inferredClassAssertions(inferences: Inferences, labels: Map[IRI, String]): List[Relation] = {
    val asserted = assertedClassAssertionsIndex(inferences.ontology)
    for {
      (individual, types) <- inferences.reasonerState.individualsDirectTypes.to(List)
      namedIndividual = NamedIndividual(IRI(individual.id))
      assertedTypes = asserted.getOrElse(namedIndividual, Set.empty)
      individualLabel = labels.getOrElse(namedIndividual.iri, namedIndividual.iri.id)
      cls <- types.filterNot(_ == BuiltIn.Top).map(typ => Class(IRI(typ.id))).filterNot(assertedTypes)
      clsLabel = labels.getOrElse(cls.iri, cls.iri.id)
    } yield Relation("Type", LabeledEntity(namedIndividual, individualLabel), LabeledEntity(cls, clsLabel))
  }

  def labelIndex(ontology: Ontology): Map[IRI, String] =
    ontology.axioms.collect { case AnnotationAssertion(RDFSLabel, iri: IRI, literal: Literal, _) =>
      iri -> lexicalForm(literal)
    }.to(Map)

  private def lexicalForm(literal: Literal): String = literal match {
    case PlainLiteral(text, _) => text
    case TypedLiteral(text, _) => text
  }

  def iri(entity: Entity): IRI = entity match {
    case Class(iri)           => iri
    case NamedIndividual(iri) => iri
    case ObjectProperty(iri)  => iri
  }

}
