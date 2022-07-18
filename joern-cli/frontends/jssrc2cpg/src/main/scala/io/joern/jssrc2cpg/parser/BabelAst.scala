package io.joern.jssrc2cpg.parser

object BabelAst {

  private val QUALIFIED_CLASS_NAME: String = BabelAst.getClass.getName

  def fromString(nodeName: String): BabelNode = {
    val clazz = Class.forName(s"$QUALIFIED_CLASS_NAME$nodeName$$")
    clazz.getField("MODULE$").get(clazz).asInstanceOf[BabelNode]
  }

  // extracted from:
  // https://github.com/babel/babel/blob/main/packages/babel-types/src/ast-types/generated/index.ts

  sealed trait BabelNode {
    override def toString: String = this.getClass.getSimpleName.stripSuffix("$")
  }

  sealed trait FlowType extends BabelNode

  sealed trait TSType extends BabelNode

  object AnyTypeAnnotation               extends FlowType
  object ArgumentPlaceholder             extends BabelNode
  object ArrayExpression                 extends BabelNode
  object ArrayPattern                    extends BabelNode
  object ArrayTypeAnnotation             extends FlowType
  object ArrowFunctionExpression         extends BabelNode
  object AssignmentExpression            extends BabelNode
  object AssignmentPattern               extends BabelNode
  object AwaitExpression                 extends BabelNode
  object BigIntLiteral                   extends BabelNode
  object BinaryExpression                extends BabelNode
  object BindExpression                  extends BabelNode
  object BlockStatement                  extends BabelNode
  object BooleanLiteral                  extends BabelNode
  object BooleanLiteralTypeAnnotation    extends FlowType
  object BooleanTypeAnnotation           extends FlowType
  object BreakStatement                  extends BabelNode
  object CallExpression                  extends BabelNode
  object CatchClause                     extends BabelNode
  object ClassAccessorProperty           extends BabelNode
  object ClassBody                       extends BabelNode
  object ClassDeclaration                extends BabelNode
  object ClassExpression                 extends BabelNode
  object ClassImplements                 extends BabelNode
  object ClassMethod                     extends BabelNode
  object ClassPrivateMethod              extends BabelNode
  object ClassPrivateProperty            extends BabelNode
  object ClassProperty                   extends BabelNode
  object ConditionalExpression           extends BabelNode
  object ContinueStatement               extends BabelNode
  object DebuggerStatement               extends BabelNode
  object DecimalLiteral                  extends BabelNode
  object DeclareClass                    extends BabelNode
  object DeclareExportAllDeclaration     extends BabelNode
  object DeclareExportDeclaration        extends BabelNode
  object DeclareFunction                 extends BabelNode
  object DeclareInterface                extends BabelNode
  object DeclareModule                   extends BabelNode
  object DeclareModuleExports            extends BabelNode
  object DeclareOpaqueType               extends BabelNode
  object DeclareTypeAlias                extends BabelNode
  object DeclareVariable                 extends BabelNode
  object DeclaredPredicate               extends BabelNode
  object Decorator                       extends BabelNode
  object Directive                       extends BabelNode
  object DirectiveLiteral                extends BabelNode
  object DoExpression                    extends BabelNode
  object DoWhileStatement                extends BabelNode
  object EmptyStatement                  extends BabelNode
  object EmptyTypeAnnotation             extends FlowType
  object EnumBooleanBody                 extends BabelNode
  object EnumBooleanMember               extends BabelNode
  object EnumDeclaration                 extends BabelNode
  object EnumDefaultedMember             extends BabelNode
  object EnumNumberBody                  extends BabelNode
  object EnumNumberMember                extends BabelNode
  object EnumStringBody                  extends BabelNode
  object EnumStringMember                extends BabelNode
  object EnumSymbolBody                  extends BabelNode
  object ExistsTypeAnnotation            extends FlowType
  object ExportAllDeclaration            extends BabelNode
  object ExportDefaultDeclaration        extends BabelNode
  object ExportDefaultSpecifier          extends BabelNode
  object ExportNamedDeclaration          extends BabelNode
  object ExportNamespaceSpecifier        extends BabelNode
  object ExportSpecifier                 extends BabelNode
  object ExpressionStatement             extends BabelNode
  object File                            extends BabelNode
  object ForInStatement                  extends BabelNode
  object ForOfStatement                  extends BabelNode
  object ForStatement                    extends BabelNode
  object FunctionDeclaration             extends BabelNode
  object FunctionExpression              extends BabelNode
  object FunctionTypeAnnotation          extends FlowType
  object FunctionTypeParam               extends BabelNode
  object GenericTypeAnnotation           extends FlowType
  object Identifier                      extends BabelNode
  object IfStatement                     extends BabelNode
  object Import                          extends BabelNode
  object ImportAttribute                 extends BabelNode
  object ImportDeclaration               extends BabelNode
  object ImportDefaultSpecifier          extends BabelNode
  object ImportNamespaceSpecifier        extends BabelNode
  object ImportSpecifier                 extends BabelNode
  object IndexedAccessType               extends FlowType
  object InferredPredicate               extends BabelNode
  object InterfaceDeclaration            extends BabelNode
  object InterfaceExtends                extends BabelNode
  object InterfaceTypeAnnotation         extends FlowType
  object InterpreterDirective            extends BabelNode
  object IntersectionTypeAnnotation      extends FlowType
  object JSXAttribute                    extends BabelNode
  object JSXClosingElement               extends BabelNode
  object JSXClosingFragment              extends BabelNode
  object JSXElement                      extends BabelNode
  object JSXEmptyExpression              extends BabelNode
  object JSXExpressionContainer          extends BabelNode
  object JSXFragment                     extends BabelNode
  object JSXIdentifier                   extends BabelNode
  object JSXMemberExpression             extends BabelNode
  object JSXNamespacedName               extends BabelNode
  object JSXOpeningElement               extends BabelNode
  object JSXOpeningFragment              extends BabelNode
  object JSXSpreadAttribute              extends BabelNode
  object JSXSpreadChild                  extends BabelNode
  object JSXText                         extends BabelNode
  object LabeledStatement                extends BabelNode
  object LogicalExpression               extends BabelNode
  object MemberExpression                extends BabelNode
  object MetaProperty                    extends BabelNode
  object MixedTypeAnnotation             extends FlowType
  object ModuleExpression                extends BabelNode
  object NewExpression                   extends BabelNode
  object Noop                            extends BabelNode
  object NullLiteral                     extends BabelNode
  object NullLiteralTypeAnnotation       extends FlowType
  object NullableTypeAnnotation          extends FlowType
  object NumberLiteral                   extends BabelNode
  object NumberLiteralTypeAnnotation     extends FlowType
  object NumberTypeAnnotation            extends FlowType
  object NumericLiteral                  extends BabelNode
  object ObjectExpression                extends BabelNode
  object ObjectMethod                    extends BabelNode
  object ObjectPattern                   extends BabelNode
  object ObjectProperty                  extends BabelNode
  object ObjectTypeAnnotation            extends FlowType
  object ObjectTypeCallProperty          extends BabelNode
  object ObjectTypeIndexer               extends BabelNode
  object ObjectTypeInternalSlot          extends BabelNode
  object ObjectTypeProperty              extends BabelNode
  object ObjectTypeSpreadProperty        extends BabelNode
  object OpaqueType                      extends BabelNode
  object OptionalCallExpression          extends BabelNode
  object OptionalIndexedAccessType       extends FlowType
  object OptionalMemberExpression        extends BabelNode
  object ParenthesizedExpression         extends BabelNode
  object PipelineBareFunction            extends BabelNode
  object PipelinePrimaryTopicReference   extends BabelNode
  object PipelineTopicExpression         extends BabelNode
  object Placeholder                     extends BabelNode
  object PrivateName                     extends BabelNode
  object Program                         extends BabelNode
  object QualifiedTypeIdentifier         extends BabelNode
  object RecordExpression                extends BabelNode
  object RegExpLiteral                   extends BabelNode
  object RegexLiteral                    extends BabelNode
  object RestElement                     extends BabelNode
  object RestProperty                    extends BabelNode
  object ReturnStatement                 extends BabelNode
  object SequenceExpression              extends BabelNode
  object SpreadElement                   extends BabelNode
  object SpreadProperty                  extends BabelNode
  object StaticBlock                     extends BabelNode
  object StringLiteral                   extends BabelNode
  object StringLiteralTypeAnnotation     extends FlowType
  object StringTypeAnnotation            extends FlowType
  object Super                           extends BabelNode
  object SwitchCase                      extends BabelNode
  object SwitchStatement                 extends BabelNode
  object SymbolTypeAnnotation            extends FlowType
  object TSAnyKeyword                    extends TSType
  object TSArrayType                     extends TSType
  object TSAsExpression                  extends BabelNode
  object TSBigIntKeyword                 extends TSType
  object TSBooleanKeyword                extends TSType
  object TSCallSignatureDeclaration      extends BabelNode
  object TSConditionalType               extends TSType
  object TSConstructSignatureDeclaration extends BabelNode
  object TSConstructorType               extends TSType
  object TSDeclareFunction               extends BabelNode
  object TSDeclareMethod                 extends BabelNode
  object TSEnumDeclaration               extends BabelNode
  object TSEnumMember                    extends BabelNode
  object TSExportAssignment              extends BabelNode
  object TSExpressionWithTypeArguments   extends TSType
  object TSExternalModuleReference       extends BabelNode
  object TSFunctionType                  extends TSType
  object TSImportEqualsDeclaration       extends BabelNode
  object TSImportType                    extends TSType
  object TSIndexSignature                extends BabelNode
  object TSIndexedAccessType             extends TSType
  object TSInferType                     extends TSType
  object TSInterfaceBody                 extends BabelNode
  object TSInterfaceDeclaration          extends BabelNode
  object TSIntersectionType              extends TSType
  object TSIntrinsicKeyword              extends TSType
  object TSLiteralType                   extends TSType
  object TSMappedType                    extends TSType
  object TSMethodSignature               extends BabelNode
  object TSModuleBlock                   extends BabelNode
  object TSModuleDeclaration             extends BabelNode
  object TSNamedTupleMember              extends BabelNode
  object TSNamespaceExportDeclaration    extends BabelNode
  object TSNeverKeyword                  extends TSType
  object TSNonNullExpression             extends BabelNode
  object TSNullKeyword                   extends TSType
  object TSNumberKeyword                 extends TSType
  object TSObjectKeyword                 extends TSType
  object TSOptionalType                  extends TSType
  object TSParameterProperty             extends BabelNode
  object TSParenthesizedType             extends TSType
  object TSPropertySignature             extends BabelNode
  object TSQualifiedName                 extends BabelNode
  object TSRestType                      extends TSType
  object TSStringKeyword                 extends TSType
  object TSSymbolKeyword                 extends TSType
  object TSThisType                      extends TSType
  object TSTupleType                     extends TSType
  object TSTypeAliasDeclaration          extends BabelNode
  object TSTypeAnnotation                extends FlowType
  object TSTypeAssertion                 extends BabelNode
  object TSTypeLiteral                   extends TSType
  object TSTypeOperator                  extends TSType
  object TSTypeParameter                 extends TSType
  object TSTypeParameterDeclaration      extends BabelNode
  object TSTypeParameterInstantiation    extends BabelNode
  object TSTypePredicate                 extends TSType
  object TSTypeQuery                     extends TSType
  object TSTypeReference                 extends TSType
  object TSUndefinedKeyword              extends TSType
  object TSUnionType                     extends TSType
  object TSUnknownKeyword                extends TSType
  object TSVoidKeyword                   extends TSType
  object TaggedTemplateExpression        extends BabelNode
  object TemplateElement                 extends BabelNode
  object TemplateLiteral                 extends BabelNode
  object ThisExpression                  extends BabelNode
  object ThisTypeAnnotation              extends FlowType
  object ThrowStatement                  extends BabelNode
  object TopicReference                  extends BabelNode
  object TryStatement                    extends BabelNode
  object TupleExpression                 extends BabelNode
  object TupleTypeAnnotation             extends FlowType
  object TypeAlias                       extends BabelNode
  object TypeAnnotation                  extends FlowType
  object TypeCastExpression              extends BabelNode
  object TypeParameter                   extends BabelNode
  object TypeParameterDeclaration        extends BabelNode
  object TypeParameterInstantiation      extends BabelNode
  object TypeofTypeAnnotation            extends FlowType
  object UnaryExpression                 extends BabelNode
  object UnionTypeAnnotation             extends FlowType
  object UpdateExpression                extends BabelNode
  object V8IntrinsicIdentifier           extends BabelNode
  object VariableDeclaration             extends BabelNode
  object VariableDeclarator              extends BabelNode
  object Variance                        extends BabelNode
  object VoidTypeAnnotation              extends FlowType
  object WhileStatement                  extends BabelNode
  object WithStatement                   extends BabelNode
  object YieldExpression                 extends BabelNode

}
