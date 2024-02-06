package io.joern.csharpsrc2cpg.datastructures

import io.joern.csharpsrc2cpg.{CSharpType, TypeMap}
import io.joern.x2cpg.datastructures.{Scope, ScopeElement}
import io.shiftleft.codepropertygraph.generated.nodes.DeclarationNew

import scala.collection.mutable

class CSharpScope(typeMap: TypeMap) extends Scope[String, DeclarationNew, ScopeType] {

  private val typesInScope = mutable.Set.empty[CSharpType]

  /** @return
    *   the surrounding type declaration if one exists.
    */
  def surroundingTypeDeclFullName: Option[String] = stack.collectFirst {
    case ScopeElement(typeLike: TypeLikeScope, _) =>
      typeLike.fullName
  }

  def getFieldsInScope: List[FieldDecl] =
    stack.collect { case ScopeElement(TypeScope(_, fields), _) => fields }.flatten.toList

  /** Works for `this`.field accesses or <currentType>.field accesses.
    */
  def findFieldInScope(fieldName: String): Option[FieldDecl] = {
    getFieldsInScope.find(_.name == fieldName)
  }

  /** @return
    *   the full name of the surrounding scope.
    */
  def surroundingScopeFullName: Option[String] = stack.collectFirst {
    case ScopeElement(NamespaceScope(fullName), _) => fullName
    case ScopeElement(MethodScope(fullName), _)    => fullName
    case ScopeElement(TypeScope(fullName, _), _)   => fullName
    case ScopeElement(typeLike: TypeLikeScope, _)  => typeLike.fullName
  }

  /** @return
    *   true if the scope is currently on the top-level, false if the scope is within some nested scope.
    */
  def isTopLevel: Boolean = stack
    .filterNot(x => x.scopeNode.isInstanceOf[NamespaceScope])
    .exists(x => x.scopeNode.isInstanceOf[MethodScope] || x.scopeNode.isInstanceOf[TypeLikeScope])

  def tryResolveTypeReference(typeName: String): Option[String] = {
    typesInScope.find(_.name.endsWith(typeName)).flatMap(typeMap.namespaceFor).map(n => s"$n.$typeName")
  }

  def tryResolveMethodInvocation(typeFullName: String, callName: String): Option[String] = {
    typesInScope.find(_.name.endsWith(typeFullName)).flatMap { t =>
      t.methods.find(_.name == callName).map { m => s"${t.name}.${m.name}" }
    }
  }

  def pushField(field: FieldDecl): Unit = {
    popScope().foreach {
      case TypeScope(fullName, fields) =>
        pushNewScope(TypeScope(fullName, fields :+ field))
      case x =>
        pushField(field)
        pushNewScope(x)
    }
  }

  /** Appends known types imported from a `using` statement into the scope.
    * @param namespace
    *   the fully qualified imported namespace.
    */
  def addImport(namespace: String): Unit = {
    val knownTypesFromNamespace = typeMap.classesIn(namespace)
    typesInScope.addAll(knownTypesFromNamespace)
  }

  /** Pops the scope, adding types from the scope if necessary.
    */
  override def pushNewScope(scopeNode: ScopeType): Unit = {
    scopeNode match
      case NamespaceScope(fullName) => typesInScope.addAll(typeMap.classesIn(fullName))
      case _                        =>
    super.pushNewScope(scopeNode)
  }

  /** Pops the scope, removing types from the scope if necessary.
    */
  override def popScope(): Option[ScopeType] = {
    super.popScope() match
      case Some(NamespaceScope(fullName)) =>
        typeMap.classesIn(fullName).foreach(typesInScope.remove)
        Some(NamespaceScope(fullName))
      case x => x
  }

  /** Returns the top of the scope, without removing it from the stack.
    */
  def peekScope(): Option[ScopeType] = {
    super.popScope() match
      case None => None
      case Some(top) =>
        super.pushNewScope(top)
        Option(top)
  }

}
