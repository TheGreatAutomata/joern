package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.CSharpOperators
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.utils.NodeBuilders.{newIdentifierNode, newOperatorCallNode}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewFieldIdentifier
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForExpressionStatement(expr: DotNetNodeInfo): Seq[Ast] = {
    val expressionNode = createDotNetNodeInfo(expr.json(ParserKeys.Expression))
    astForExpression(expressionNode)
  }

  def astForExpression(expr: DotNetNodeInfo): Seq[Ast] = {
    expr.node match
      case _: UnaryExpr                 => astForUnaryExpression(expr)
      case _: BinaryExpr                => astForBinaryExpression(expr)
      case _: LiteralExpr               => astForLiteralExpression(expr)
      case InvocationExpression         => astForInvocationExpression(expr)
      case AwaitExpression              => astForAwaitExpression(expr)
      case ObjectCreationExpression     => astForObjectCreationExpression(expr)
      case SimpleMemberAccessExpression => astForSimpleMemberAccessExpression(expr)
      case _: IdentifierNode            => astForIdentifier(expr) :: Nil
      case _                            => notHandledYet(expr)
  }

  private def astForAwaitExpression(awaitExpr: DotNetNodeInfo): Seq[Ast] = {
    /* fullName is the name in case of STATIC_DISPATCH */
    val node =
      callNode(awaitExpr, awaitExpr.code, CSharpOperators.await, CSharpOperators.await, DispatchTypes.STATIC_DISPATCH)
    val argAsts = astForNode(awaitExpr.json(ParserKeys.Expression))
    Seq(callAst(node, argAsts))
  }

  protected def astForLiteralExpression(_literalNode: DotNetNodeInfo): Seq[Ast] = {
    Seq(Ast(literalNode(_literalNode, code(_literalNode), nodeTypeFullName(_literalNode))))
  }

  protected def astForOperand(operandNode: DotNetNodeInfo): Seq[Ast] = {
    operandNode.node match {
      case IdentifierName =>
        List(scope.findFieldInScope(nameFromNode(operandNode)), scope.lookupVariable(nameFromNode(operandNode))) match {
          case List(Some(_), None) => astForSimpleMemberAccessExpression(operandNode)
          case _                   => astForNode(operandNode)
        }
      case _ => astForNode(operandNode)
    }
  }

  protected def astForUnaryExpression(unaryExpr: DotNetNodeInfo): Seq[Ast] = {
    val operatorToken = unaryExpr.json(ParserKeys.OperatorToken)(ParserKeys.Value).str
    val operatorName = operatorToken match
      case "+" => Operators.plus
      case "-" => Operators.minus
      case "++" =>
        if (unaryExpr.node.getClass == PostIncrementExpression.getClass) Operators.postIncrement
        else Operators.preIncrement
      case "--" =>
        if (unaryExpr.node.getClass == PostDecrementExpression.getClass) Operators.postDecrement
        else Operators.preDecrement
      case "~" => Operators.not
      case "!" => Operators.logicalNot
      case "&" => Operators.addressOf

    val args    = createDotNetNodeInfo(unaryExpr.json(ParserKeys.Operand))
    val argsAst = astForOperand(args)

    Seq(
      callAst(createCallNodeForOperator(unaryExpr, operatorName, typeFullName = Some(nodeTypeFullName(args))), argsAst)
    )
  }

  protected def astForBinaryExpression(binaryExpr: DotNetNodeInfo): Seq[Ast] = {
    val operatorToken = binaryExpr.json(ParserKeys.OperatorToken)(ParserKeys.Value).str
    val operatorName = operatorToken match
      case "+"   => Operators.addition
      case "-"   => Operators.subtraction
      case "*"   => Operators.multiplication
      case "/"   => Operators.division
      case "%"   => Operators.modulo
      case "=="  => Operators.equals
      case "!="  => Operators.notEquals
      case "&&"  => Operators.logicalAnd
      case "||"  => Operators.logicalOr
      case "="   => Operators.assignment
      case "+="  => Operators.assignmentPlus
      case "-="  => Operators.assignmentMinus
      case "*="  => Operators.assignmentMultiplication
      case "/="  => Operators.assignmentDivision
      case "%="  => Operators.assignmentModulo
      case "&="  => Operators.assignmentAnd
      case "|="  => Operators.assignmentOr
      case "^="  => Operators.assignmentXor
      case ">>=" => Operators.assignmentLogicalShiftRight
      case "<<=" => Operators.assignmentShiftLeft
      case ">"   => Operators.greaterThan
      case "<"   => Operators.lessThan
      case ">="  => Operators.greaterEqualsThan
      case "<="  => Operators.lessEqualsThan
      case "|"   => Operators.or
      case "&"   => Operators.and
      case "^"   => Operators.xor
      case x =>
        logger.warn(s"Unhandled operator '$x' for ${code(binaryExpr)}")
        CSharpOperators.unknown

    val args = astForOperand(createDotNetNodeInfo(binaryExpr.json(ParserKeys.Left))) ++: astForOperand(
      createDotNetNodeInfo(binaryExpr.json(ParserKeys.Right))
    )

    val cNode =
      createCallNodeForOperator(binaryExpr, operatorName, typeFullName = Some(getTypeFullNameFromAstNode(args)))
    Seq(callAst(cNode, args))
  }

  /** Handles the `= ...` part of the equals value clause, thus this only contains an RHS.
    */
  protected def astForEqualsValueClause(clause: DotNetNodeInfo): Seq[Ast] = {
    val rhsNode = createDotNetNodeInfo(clause.json(ParserKeys.Value))
    astForNode(rhsNode)
  }

  // TODO: This method is becoming a mess...
  private def astForInvocationExpression(invocationExpr: DotNetNodeInfo): Seq[Ast] = {
    val arguments = astForArgumentList(createDotNetNodeInfo(invocationExpr.json(ParserKeys.ArgumentList)))

    val expression = createDotNetNodeInfo(invocationExpr.json(ParserKeys.Expression))
    val name       = nameFromNode(expression)

    val (receiver, baseTypeFullName) = expression.node match
      case SimpleMemberAccessExpression =>
        val baseNode = createDotNetNodeInfo(
          createDotNetNodeInfo(invocationExpr.json(ParserKeys.Expression)).json(ParserKeys.Expression)
        )
        val baseNodeName   = nameFromNode(baseNode)
        val staticTypeRef  = scope.tryResolveTypeReference(baseNodeName)
        val baseIdentifier = astForIdentifier(baseNode, staticTypeRef.getOrElse(Defines.Any))
        val _typeFullName  = getTypeFullNameFromAstNode(Seq(baseIdentifier))

        if (_typeFullName != Defines.Any) {
          val _identifierNode =
            identifierNode(baseNode, nameFromNode(baseNode), code(baseNode), _typeFullName)
          (Option(Ast(_identifierNode)), Option(_identifierNode.typeFullName))
        } else if (staticTypeRef.isDefined) {
          (Option(baseIdentifier), Option(staticTypeRef.get))
        } else {
          (Option(baseIdentifier), Option(_typeFullName))
        }
      case _ => (None, None)

    lazy val partialFullName = baseTypeFullName match
      case Some(typeFullName) =>
        s"$typeFullName.$name"
      case _ =>
        s"${Defines.UnresolvedNamespace}.$name"

    val returnType =
      scope.tryResolveMethodReturn(baseTypeFullName.getOrElse(scope.surroundingTypeDeclFullName.getOrElse("")), name);

    val parameterSignature = scope
      .tryResolveMethodSignature(baseTypeFullName.getOrElse(scope.surroundingTypeDeclFullName.getOrElse("")), name)
      .getOrElse(Defines.UnresolvedSignature)
    val typeFullName = returnType.getOrElse(Defines.Any)

    val methodSignature = s"${returnType.getOrElse(Defines.Unknown)}($parameterSignature)"
    val defaultFullName = s"$partialFullName:$methodSignature"
    val (methodFullName, dispatchType) = baseTypeFullName match {
      case Some(baseFullName) if scope.tryResolveMethodInvocation(baseFullName, name).isDefined =>
        val methodMetaData = scope.tryResolveMethodInvocation(baseFullName, name).get
        s"$baseFullName.${methodMetaData.name}:$methodSignature" -> (if methodMetaData.isStatic then
                                                                       DispatchTypes.STATIC_DISPATCH
                                                                     else DispatchTypes.DYNAMIC_DISPATCH)
      case None
          if scope.surroundingTypeDeclFullName.isDefined && scope
            .tryResolveMethodInvocation(scope.surroundingTypeDeclFullName.get, name)
            .isDefined =>
        val baseTypeFullName = scope.surroundingTypeDeclFullName.get
        val methodMetaData   = scope.tryResolveMethodInvocation(baseTypeFullName, name).get
        s"$baseTypeFullName.${methodMetaData.name}:$methodSignature" -> (if methodMetaData.isStatic then
                                                                           DispatchTypes.STATIC_DISPATCH
                                                                         else DispatchTypes.DYNAMIC_DISPATCH)
      case _ => defaultFullName -> DispatchTypes.STATIC_DISPATCH
    }

    val _callAst = callAst(
      callNode(
        invocationExpr,
        code(invocationExpr),
        name,
        methodFullName,
        dispatchType,
        Option(methodSignature),
        Option(typeFullName)
      ),
      arguments,
      receiver
    )
    Seq(_callAst)
  }

  protected def astForSimpleMemberAccessExpression(accessExpr: DotNetNodeInfo): Seq[Ast] = {
    val fieldIdentifierName = nameFromNode(accessExpr)

    val fieldInScope = scope.findFieldInScope(fieldIdentifierName)

    val typeFullName = fieldInScope.map(_.typeFullName).getOrElse(Defines.Any)
    val identifierName =
      if (fieldInScope.nonEmpty && fieldInScope.get.isStatic) scope.surroundingTypeDeclFullName.getOrElse(Defines.Any)
      else "this"

    val identifier = newIdentifierNode(identifierName, typeFullName)

    val fieldIdentifier = NewFieldIdentifier()
      .code(fieldIdentifierName)
      .canonicalName(fieldIdentifierName)
      .lineNumber(accessExpr.lineNumber)
      .columnNumber(accessExpr.columnNumber)

    val fieldAccessCode = s"$identifierName.$fieldIdentifierName"

    val fieldAccess =
      newOperatorCallNode(
        Operators.fieldAccess,
        fieldAccessCode,
        Some(typeFullName).orElse(Some(Defines.Any)),
        accessExpr.lineNumber,
        accessExpr.columnNumber
      )

    val identifierAst = Ast(identifier)
    val fieldIdentAst = Ast(fieldIdentifier)

    Seq(callAst(fieldAccess, Seq(identifierAst, fieldIdentAst)))
  }

  def astForObjectCreationExpression(objectCreation: DotNetNodeInfo): Seq[Ast] = {
    val dispatchType = DispatchTypes.STATIC_DISPATCH
    val typeFullName = nodeTypeFullName(objectCreation)
    val arguments    = astForArgumentList(createDotNetNodeInfo(objectCreation.json(ParserKeys.ArgumentList)))
    // TODO: Handle signature
    val signature      = None
    val name           = Defines.ConstructorMethodName
    val methodFullName = s"$typeFullName.$name"
    val _callNode = callNode(
      objectCreation,
      code(objectCreation),
      name,
      methodFullName,
      dispatchType,
      signature,
      Option(typeFullName)
    )

    Seq(callAst(_callNode, arguments, Option(Ast(thisNode))))
  }

  private def astForArgumentList(argumentList: DotNetNodeInfo): Seq[Ast] = {
    argumentList
      .json(ParserKeys.Arguments)
      .arr
      .map(createDotNetNodeInfo)
      .flatMap(astForExpressionStatement)
      .toSeq
  }

}
