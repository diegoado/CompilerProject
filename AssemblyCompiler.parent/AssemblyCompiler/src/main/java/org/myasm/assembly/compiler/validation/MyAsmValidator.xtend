package org.myasm.assembly.compiler.validation

import java.util.Arrays
import java.util.ArrayList
import java.util.Collections
import java.util.Comparator
import java.util.List
import java.util.Map
import java.util.HashMap
import java.util.HashSet
import java.util.Set
import java.util.TreeSet

import org.eclipse.xtext.validation.Check
import org.eclipse.emf.ecore.EObject

import org.myasm.assembly.compiler.myAsm.ArrayInitializer
import org.myasm.assembly.compiler.myAsm.Attribute
import org.myasm.assembly.compiler.myAsm.AssignmentStatement
import org.myasm.assembly.compiler.myAsm.CastExpression
import org.myasm.assembly.compiler.myAsm.ClassDeclaration
import org.myasm.assembly.compiler.myAsm.ClassInstanceCreationExpression
import org.myasm.assembly.compiler.myAsm.CompilationUnit
import org.myasm.assembly.compiler.myAsm.DeclarationBody
import org.myasm.assembly.compiler.myAsm.Expression
import org.myasm.assembly.compiler.myAsm.FormalParameter
import org.myasm.assembly.compiler.myAsm.FloatingIntLiteral
import org.myasm.assembly.compiler.myAsm.InterfaceDeclaration
import org.myasm.assembly.compiler.myAsm.IntegerLiteral
import org.myasm.assembly.compiler.myAsm.LogicalExpression
import org.myasm.assembly.compiler.myAsm.Method
import org.myasm.assembly.compiler.myAsm.MethodDeclarator
import org.myasm.assembly.compiler.myAsm.MethodInvocation
import org.myasm.assembly.compiler.myAsm.MyAsmPackage
import org.myasm.assembly.compiler.myAsm.NumericExpression
import org.myasm.assembly.compiler.myAsm.ObjectLiteral
import org.myasm.assembly.compiler.myAsm.ObjectType
import org.myasm.assembly.compiler.myAsm.Variable
import org.myasm.assembly.compiler.myAsm.VariableDeclarator
import org.myasm.assembly.compiler.myAsm.VariableInitializer
import org.myasm.assembly.compiler.myAsm.TestingExpression
import org.myasm.assembly.compiler.myAsm.TypeDeclaration
import org.myasm.assembly.compiler.myAsm.ReturnStatement
import org.myasm.assembly.compiler.myAsm.SwitchStatement

class MyAsmValidator extends AbstractMyAsmValidator {

    private List<String> classes;
    private List<String> interfaces;

    private Map<String, Set<String>> extended;
    private Map<String, Set<String>> implemented;

    private Map<String, List<Method>>    methods;
    private Map<String, List<Attribute>> attributes;
    private Map<String, Set<Method>> inheritedMethods;

    private List<String> finalCls;
    private Map<String, Set<String>> attrScope;
    
    @Check
    def programMapper(CompilationUnit program) {
        classes = new ArrayList<String>();
        interfaces = new ArrayList<String>();

        extended = new HashMap<String, Set<String>>();
        implemented = new HashMap<String, Set<String>>();

        methods = new HashMap<String, List<Method>>();
        attributes = new HashMap<String, List<Attribute>>();
        inheritedMethods = new HashMap<String, Set<Method>>();

        finalCls  = new ArrayList<String>();
        attrScope = new HashMap<String, Set<String>>();

        addStringType();
        for (TypeDeclaration declaration : program.getDeclarations()) {
            extended        .put(declaration.name, new HashSet<String>());
            inheritedMethods.put(declaration.name, new TreeSet<Method>(getMethodComparator()));

            methods   .put(declaration.name, new ArrayList<Method>());
            attributes.put(declaration.name, new ArrayList<Attribute>());

            attrScope .put(declaration.name, new HashSet<String>());

            if (declaration instanceof ClassDeclaration) {
                classes    .add(declaration.name);
                implemented.put(declaration.name, new HashSet<String>());
                checkClassDeclaration(declaration);
            } else if (declaration instanceof InterfaceDeclaration) {
                interfaces.add(declaration.name);
                checkInterfaceDeclaration(declaration);
            }
            checkMethodDeclaration   (declaration);
            checkAttributeDeclaration(declaration);

            if (declaration instanceof ClassDeclaration) {
                checkClassInheritance(declaration);
            }
        }
    }

    def checkClassDeclaration(ClassDeclaration clazz) {
        if (Collections.frequency(classes, clazz.name) > 1) {
            error("The class " + clazz.name + " already declared.",
            clazz,
            MyAsmPackage.Literals.TYPE_DECLARATION__NAME);
        }
        if (clazz.modifiers != null) {
            if (clazz.modifiers.contains("abstract") && clazz.modifiers.contains("final")) {
                error("Only one of the modifiers can be used: abstract or final.",
                clazz,
                MyAsmPackage.Literals.TYPE_DECLARATION__MODIFIERS);
            } else if (Collections.frequency(clazz.modifiers, "abstract") > 1 ||
                    Collections.frequency(clazz.modifiers, "final") > 1) {
                error("Only one instance of the final or abstract modifier is allowed.",
                clazz,
                MyAsmPackage.Literals.TYPE_DECLARATION__MODIFIERS);
            }
            if (clazz.modifiers.contains("final")) {
                finalCls.add(clazz.name);
            }
        }
        checkClassExtends    (clazz);
        checkClassImplements (clazz);
    }

    def checkInterfaceDeclaration(InterfaceDeclaration declaration) {
        if (Collections.frequency(interfaces, declaration.name) > 1) {
            error("The interface " + declaration.name + " already declared.",
            declaration,
            MyAsmPackage.Literals.TYPE_DECLARATION__NAME);
        }
        if (declaration.modifiers != null && Collections.frequency(declaration.modifiers, "abstract") > 1) {
            error("Only one instance abstract modifier is allowed.",
            declaration,
            MyAsmPackage.Literals.TYPE_DECLARATION__MODIFIERS);
        }
        checkInterfaceExtends(declaration);
    }

    def checkMethodDeclaration(TypeDeclaration declaration) {
        for (EObject definition : declaration.body.getDeclarations()) {
            if (definition instanceof Method) {
                val type = definition.signature.getType();
                val modifiers = definition.signature.getModifiers();

                //TODO(diegoadolfo): check method modifiers

                if (modifiers.contains("abstract")) {
                    if (definition.body != null) {
                        error("An abstract method must have an empty body.",
                        definition.signature,
                        MyAsmPackage.Literals.METHOD_HEADER__MODIFIERS);
                    } else if (declaration instanceof ClassDeclaration &&
                            !declaration.modifiers.contains("abstract")) {
                        error("An abstract method, can only be declared in an abstract class.",
                        definition.signature,
                        MyAsmPackage.Literals.METHOD_HEADER__MODIFIERS);
                    }
                } else if (definition.body == null && !modifiers.contains("abstract")
                        && declaration instanceof ClassDeclaration) {
                    error("An non abstract method must have a body.",
                    definition.signature,
                    MyAsmPackage.Literals.METHOD_HEADER__MODIFIERS);
                } else if (type instanceof ObjectType) {
                    if(methods.get(type.name) == null) {
                        error("Method retult type " + type.name + " unreachable.",
                        definition.signature,
                        MyAsmPackage.Literals.METHOD_HEADER__TYPE);
                    }
                }
                checkMethodHeader(declaration.name, definition.signature.getHeader());

                if (declaration instanceof InterfaceDeclaration && !modifiers.contains("abstract")) {
                    definition.signature.modifiers.add("abstract");
                }
                methods.get(declaration.name).add(definition);
                if (declaration instanceof ClassDeclaration     && !modifiers.contains("abstract")) {
                    checkMethodBody (declaration.name, definition);
                }
            }
        }
    }

    def checkAttributeDeclaration(TypeDeclaration owner) {
        var String type;

        for (EObject block : owner.body.declarations) {
            if (block instanceof Attribute) {
                for (VariableDeclarator variable : block.declarations) {
                    if (attrScope.get(owner.name).contains(variable.facade.name)) {
                        error("Class attribute " + variable.facade.name + " already declared.",
                        variable,
                        MyAsmPackage.Literals.VARIABLE_DECLARATOR__FACADE);
                    }
                    if (block.type instanceof ObjectType) {
                        type = (block.type as ObjectType).name;

                        if (attributes.get(type) == null) {
                            error("Attribute type " + type + " unreachable.",
                            block,
                            MyAsmPackage.Literals.ATTRIBUTE__TYPE);
                        }
                    } else {
                        type = block.type.eClass.name;
                    }
                    checkVariableType(owner.name, variable.definition, type);

                    attributes.get(owner.name).add(block);
                    attrScope .get(owner.name).add(variable.facade.name)
                }
            }
        }
    }

    def checkClassExtends(ClassDeclaration clazz) {
        if (clazz.getExtends() != null) {
            val superClass = clazz.getExtends().name;

            if (interfaces.contains(superClass)) {
                error(superClass + " is a interface.", clazz,
                MyAsmPackage.Literals.CLASS_DECLARATION__EXTENDS);
            } else if (!classes.contains(superClass)) {
                error("The class " + superClass + " not declared.",
                clazz,
                MyAsmPackage.Literals.CLASS_DECLARATION__EXTENDS);
            } else if (clazz.name.equals(superClass)) {
                error("The class cannot extends itself.",
                clazz,
                MyAsmPackage.Literals.CLASS_DECLARATION__EXTENDS);
            } else {
                extended.get(clazz.name).add(superClass);
                addInheritedMethods(superClass, clazz.name);
            }
        }
    }

    def checkInterfaceExtends(InterfaceDeclaration declaration) {
        val interfaceList = declaration.getExtends()

        if (interfaceList != null) {
            for (String interfacce : interfaceList.getInterfaces()) {
                if (classes.contains(interfacce)) {
                    error(interfacce + " is a class.", declaration,
                    MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else if (!interfaces.contains(interfacce)) {
                    error("The interface " + interfacce + " not declared.",
                    declaration,
                    MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else if (declaration.name.equals(interfacce)) {
                    error("The interface cannot extends itself.",
                    declaration,
                    MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else if (extended.get(declaration.name).contains(interfacce)) {
                    error("The interface " + declaration.name + " already extends interface " + interfacce + ".",
                    declaration,
                    MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else {
                    extended.get(declaration.name).add(interfacce);
                    addInheritedMethods(interfacce, declaration.name);
                }
            }
        }
    }

    def checkClassImplements(ClassDeclaration clazz) {
        val interfaceList = clazz.getImplements();

        if (interfaceList != null) {
            for (String interfacce : interfaceList.getInterfaces()) {
                if (classes.contains(interfacce)) {
                    error(interfacce + " is a class.",
                    clazz,
                    MyAsmPackage.Literals.CLASS_DECLARATION__IMPLEMENTS);
                } else if (!interfaces.contains(interfacce)) {
                    error("The interface " + interfacce + " not declared.",
                    clazz, MyAsmPackage.Literals.CLASS_DECLARATION__IMPLEMENTS);
                } else if (implemented.get(clazz.name).contains(interfacce)) {
                    error("The class " + clazz.name + " already implements interface " + interfacce + ".",
                    clazz,
                    MyAsmPackage.Literals.CLASS_DECLARATION__IMPLEMENTS);
                } else {
                    implemented.get(clazz.name).add(interfacce);
                    addInheritedMethods(interfacce, clazz.name);
                }
            }
        }
    }

    def checkClassInheritance(ClassDeclaration clazz) {
        if (clazz.getExtends() != null && finalCls .contains(clazz.getExtends().name)) {
            error("The class " + clazz.getExtends().name + " is final, it cannot be inherited.",
            clazz.getExtends(),
            MyAsmPackage.Literals.SUPER_CLASS__NAME);
        }
        val classMethods = getClassMethods(clazz);
        var Set<Method> methods = new TreeSet<Method>(getMethodComparator());

        if ((clazz.modifiers   != null && !clazz.modifiers.contains("abstract")) ||
                clazz.modifiers == null) {

            methods.addAll(inheritedMethods
            .get(clazz.name) .filter[it.signature.modifiers.contains("abstract")]);
            if (!classMethods.containsAll(methods)) {
                methods.clear();

                if (clazz.getExtends() != null) {
                    methods.addAll(inheritedMethods
                    .get(clazz.getExtends().name).filter[it.signature.modifiers.contains("abstract")]);
                }
                if (!classMethods.containsAll(methods)) {
                    error("The class " + clazz.name + " must implement all superclass abstract methods.",
                    clazz.getExtends(),
                    MyAsmPackage.Literals.SUPER_CLASS__NAME);
                } else {
                    error("The class " + clazz.name + " must implement all interfaces methods.",
                    clazz.getImplements(),
                    MyAsmPackage.Literals.INTERFACE_LIST__INTERFACES);
                }
            }
        }
    }

    def checkMethodHeader(String owner, MethodDeclarator header) {
        var List<String> paramsType = new ArrayList<String>();

        for (FormalParameter param : header.getParams()) {
            if (param.type instanceof ObjectType) {
                val type = param.type as ObjectType;

                paramsType.add(type.name);
                if(methods.get(type.name) == null) {
                    error("Method param type " + type.name + " unreachable.",
                    param,
                    MyAsmPackage.Literals.FORMAL_PARAMETER__TYPE);
                }
            } else {
                paramsType.add(param.type.eClass.name);
            }
        }
        val methods = methods.get(owner)
        .filter[it.signature.header.name.equals(header.name)]

        for (Method method : methods) {
            val methodParamsType = extractMethodParamsType(method.signature.header)

            if (paramsType.equals(methodParamsType)) {
                error("Method " + header.name + " already declared.",
                header,
                MyAsmPackage.Literals.METHOD_DECLARATOR__NAME);
            }
        }
    }

    def checkMethodBody(String owner, Method method) {
        val body = method.body as DeclarationBody;
        val returns = checkStatementBlock(owner, body.declarations, new ArrayList<Variable>());

        var String  resultType;
        if (method.signature.type instanceof ObjectType) {
            resultType = (method.signature.type as ObjectType).name;
        } else {
            resultType = method.signature.type.eClass.name;
        }
        if (!returns.isEmpty() && resultType == "VoidType") {
            error("Cannot return a value from a method with void result type.",
            method.signature,
            MyAsmPackage.Literals.METHOD_HEADER__TYPE)
        } else if (returns.isEmpty() && resultType != "VoidType") {
            error("Return a value expected from a method with " + resultType + " as result type.",
            method.signature,
            MyAsmPackage.Literals.METHOD_HEADER__TYPE)
        } else {
            for (String type : returns) {
                if (!isCompatibleType(resultType, type)) {
                    error("Type mismatch. Expected: " + resultType + ". Found: " + type + ".",
                    method.signature,
                    MyAsmPackage.Literals.METHOD_HEADER__TYPE)
                    return;
                }
            }
        }
    }

    def checkMethodInvacation(String owner, MethodInvocation method) {
        var List<Method> candidatesMethods = new ArrayList<Method>();

        // Owner class methods
        candidatesMethods.addAll(methods.get(owner)
        .filter[it.signature.header.name.equals(method.name)]);
        // Owner inherited class methods
        candidatesMethods.addAll(inheritedMethods.get(owner)
        .filter[it.signature.header.name.equals(method.name)]);

        if (candidatesMethods.isNullOrEmpty()) {
            error("Unreachable method " + method.name + " in class " + owner + ".",
            method,
            MyAsmPackage.Literals.METHOD_INVOCATION__NAME);

            //Method type unreachable
            return "!method";
        }
        var List<String> params = new ArrayList<String>();

        if (method.params != null) {
            for (Expression expr : method.params.declarations) {
                var String result = evaluateType(owner, expr, new ArrayList<Variable>()) as String;
                params.add(result);
            }
        }
        for (Method candidate : candidatesMethods) {
            val  methodParams = extractMethodParamsType(candidate.signature.header)

            if (params.equals(methodParams)) {
                if (candidate.signature.type instanceof ObjectType) {
                    return (candidate.signature.type as ObjectType).name;
                } else {
                    return candidate.signature.type.eClass.name;
                }
            }
        }
        error("Unreachable method " + method.name + " in class " + owner + " with input parameters.",
        method,
        MyAsmPackage.Literals.METHOD_INVOCATION__PARAMS);

        //Method type unreachable
        return "!method";
    }

    def checkVariableDeclaration(String owner, Variable variable, Set<String> varNames, List<Variable> scope) {
        var String type;

        for (VariableDeclarator declaration : variable.declarations) {
            if (varNames.contains(declaration.facade.name)) {
                error("Variable " + declaration.facade.name + " already declared.",
                declaration,
                MyAsmPackage.Literals.VARIABLE_DECLARATOR__FACADE);
            }
            if (variable.type instanceof ObjectType) {
                type = (variable.type as ObjectType).name;

                if (attributes.get(type) == null) {
                    error("Variable type " + type + " unreachable.",
                    variable,
                    MyAsmPackage.Literals.VARIABLE__TYPE);
                }
            } else {
                type = variable.type.eClass.name;
            }
            checkVariableType(owner, declaration.definition, type, scope);
            varNames.add(declaration.facade.name);
        }
    }

    def checkVariableType(String owner, VariableInitializer definition, String type) {
        if (definition instanceof Expression) {
            var String result = evaluateType(owner, definition, new ArrayList<Variable>());

            if (result != null && !result.equals("!method") && !result.equals("!expr") &&
                    !result.contains("Type") && attributes.get(result) == null) {
                result  = deriveVariableTypeFromScope(owner, result);

                if (result == null) {
                    error("Variable not declared in spoce of class " + owner + ".",
                    definition, null, -1);
                }
            }
            if (result != null && !result.equals("!method") && !result.equals("!expr") &&
                    !isCompatibleType(type, result)) {
                error("Fail to derive a compatible type to " + type + " and " + result + ", types are not compatible.",
                definition, null, -1);
            }
        } else if (definition instanceof ArrayInitializer) {
        }
    }

    def checkVariableType(String owner, VariableInitializer definition, String type, List<Variable> scope) {
        if (definition instanceof Expression) {
            var String result = evaluateType(owner, definition, scope);

            if (result != null && !result.equals("!method") && !result.equals("!expr") &&
                    !result.contains("Type") && attributes.get(result) == null) {
                result  = deriveVariableTypeFromScope(owner, result, scope);

                if (result == null) {
                    error("Variable not declared in spoce of class " + owner + ".",
                    definition, null, -1);
                }
            }
            if (result != null && !result.equals("!method") && !result.equals("!expr") &&
                    !isCompatibleType(type, result)) {
                error("Fail to derive a compatible type to " + type + " and " + result + ", types are not compatible.",
                definition, null, -1);
            }
        } else if (definition instanceof ArrayInitializer) {
        }
    }

    def checkStatementBlock(String owner, List<EObject> statements, List<Variable> scope) {
        var List<String> returns   = new ArrayList<String>();
        var Set <String> varNames  = new HashSet  <String>();

        for (EObject statement : statements) {
            if (statement instanceof Variable) {
                checkVariableDeclaration(owner, statement, varNames, scope);
                scope.add(statement);
            } else if (statement instanceof AssignmentStatement) {
                checkAssignmentStatement(owner, statement, scope);
            } else if (statement instanceof Expression) {
            } else if (statement instanceof SwitchStatement) {
                checkSwitchStatement    (owner, statement, scope);
            } else if (statement instanceof ReturnStatement) {
                val result  = checkReturnStatement(owner, statement, scope);
                if (result != null && !result.equals("!method") && !result.equals("!expr")) {
                    returns.add(result);
                }
            } else {
            }
        }
        return returns;
    }

    def checkAssignmentStatement(String owner, AssignmentStatement statement, List<Variable> scope) {
        var String varType = deriveVariableTypeFromScope(owner, statement.left, scope);

        if (varType == null) {
            error("Unreachable variable or attribute " + statement.left + " in class " + owner + ".",
            statement,
            MyAsmPackage.Literals.ASSIGNMENT_STATEMENT__LEFT);
        } else {
            var String expType = evaluateType(owner, statement.right, scope);

            if (expType != null && !expType.equals("!method") && !expType.equals("!expr") &&
                    !expType.contains("Type") && attributes.get(expType) == null) {
                expType = deriveVariableTypeFromScope(owner, expType, scope);

                if (expType == null) {
                    error("Variable not declared in method spoce of class " + owner + ".",
                    statement.right, null, -1);
                }
            }
            if (expType != null && !expType.equals("!method") && !expType.equals("!expr") &&
                    !isCompatibleType(varType, expType)) {
                error("Type mismatch. Expected: " + varType + ". Found: " + expType,
                statement.right, null, -1);
            }
            //TODO(diegoadolfo): check operator support
        }
    }

    def checkSwitchStatement(String owner, SwitchStatement statement, List<Variable> scope) {
        var String switchType = evaluateType(owner, statement.expression, scope);

        if (switchType != null) {
            if ((!switchType.equals("IntType") && !switchType.equals("String") &&
                    Arrays .asList("LongType", "FloatType", "DoubleType", "BooleanType", "NullType")
                    .contains(switchType)) || methods.get(switchType) != null) {

                error("Type mismatch. Expected: IntType or String. Found: " + switchType + ".",
                statement.expression, null, -1);
            } else if (!switchType.equals("IntType") && !switchType.equals("String")
                    && !switchType.equals("!method") && !switchType.equals("!expr")) {
                switchType = deriveVariableTypeFromScope(owner, switchType, scope);

                if (switchType == null) {
                    error("Variable not declared in method spoce of class " + owner + ".",
                    statement.expression, null, -1);
                } else if (!switchType.equals("IntType") && !switchType.equals("String")) {
                    error("Type mismatch. Expected: IntType or String. Found: " + switchType + ".",
                    statement.expression, null, -1);
                } else {
                    for (Expression constant : statement.constants) {
                        var String  caseType = evaluateType(owner, constant, scope);

                        if (caseType != null && !caseType.contains("Type") && !caseType.equals("!method") &&
                                !caseType.equals("!expr") && methods.get(caseType) == null) {
                            caseType = deriveVariableTypeFromScope(owner, caseType, scope);

                            if (caseType == null) {
                                error("Variable not declared in method spoce of class " + owner + ".",
                                constant, null, -1);
                            }
                        }
                        if (caseType !=null && !caseType.equals("!method") && !caseType.equals("!expr") &&
                                !switchType.equals(caseType)) {
                            error("Type mismatch. Expected: " + switchType + ". Found: " + caseType,
                            constant, null, -1);
                        }
                    }
                }
            }
        }
    }

    def checkReturnStatement(String owner, ReturnStatement statement, List<Variable> scope) {
        if (statement.expression == null) {
            return null;
        }
        var String exprType = evaluateType(owner, statement.expression, scope);

        if (exprType != null && !exprType.equals("!method") && !exprType.equals("!expr") &&
                !exprType.contains("Type") && attributes.get(exprType) == null) {
            exprType  = deriveVariableTypeFromScope(owner, exprType, scope);

            if (exprType == null) {
                error("Variable not declared in spoce of class " + owner + ".",
                statement.expression, null, -1);
            }
        }
        return exprType;
    }

    def checkNumericExpression(String owner, NumericExpression expr, List<Variable> scope) {
        var String exprType;
        var String lType; var String rType;

        if (expr.left != null && expr.right != null) {
            lType  = evaluateExpressionTypeSide(owner, expr.left, scope);
            rType  = evaluateExpressionTypeSide(owner, expr.right, scope);

            if (lType != null && !lType.equals("!method") && !lType.equals("!expr") &&
                    rType != null && !rType.equals("!method") && !rType.equals("!expr")) {

                if (!isCompatibleType(lType, rType) && !isCompatibleType(rType, lType)) {
                    error("Type mismatch. The types of expression operands are incompatible.",
                    expr, null, -1);
                }
                exprType = if (lType.equals(rType)) lType else superType(lType, rType);

                if (exprType != "String" && attributes.get(exprType) != null) {
                    error("Type mismatch. Expected: A numeric type. Found: " + exprType + ".",
                    expr, null, -1);
                } else if (exprType == "String" && !expr.operator.equals("+")) {
                    error("Expression operator does not support type String.",
                    expr,
                    MyAsmPackage.Literals.NUMERIC_EXPRESSION__OPERATOR);
                } else if (exprType.equals("BooleanType")) {
                    error("Type mismatch. Expected: A numeric type. Found: BooleanType.",
                    expr, null, -1);
                } else {
                    return exprType;
                }
            }
        }
        return "!expr";
    }

    def evaluateExpressionTypeSide(String owner, Expression expr, List<Variable> scope) {
        var String exprType = evaluateType(owner, expr, scope) as String;

        if (exprType != null && !exprType.equals("!method") && !exprType.equals("!expr") &&
                !exprType.contains("Type") && attributes.get(exprType) == null) {

            exprType = deriveVariableTypeFromScope(owner, exprType, scope);

            if (exprType == null) {
                error("Variable not declared in method spoce of class " + owner + ".",
                expr, null, -1);
            }
        }
        return exprType;
    }

    def addStringType() {
        methods.put("String", new ArrayList<Method>());
        attributes.put("String", new ArrayList<Attribute>());
    }

    def extractMethodParamsType(MethodDeclarator header) {
        var List<String> paramsType = new ArrayList<String>();

        for (FormalParameter param : header.getParams()) {
            if (param.type instanceof ObjectType) {
                val type = param.type as ObjectType;
                paramsType.add(type.name);
            } else {
                paramsType.add(param.type.eClass.name);
            }
        }
        return paramsType;
    }

    def getClassMethods(ClassDeclaration clazz) {
        var Set<Method> classMethods = new TreeSet<Method>(getMethodComparator());

        classMethods.addAll(methods.get(clazz.name));
        return classMethods;
    }

    def addInheritedMethods(String src, String dest) {
        val cmp = getMethodComparator()

        for (Method method : methods.get(src)) {
            if (inheritedMethods.get(dest).contains(method)) {
                val result = inheritedMethods.get(dest)
                .removeIf[cmp.compare(it, method) == 0 && it.signature.modifiers.contains("abstract")]
                if (result) {
                    inheritedMethods.get(dest).add(method);
                }
            } else {
                inheritedMethods.get(dest).add(method);
            }
        }
        for (Method method : inheritedMethods.get(src)) {
            if (inheritedMethods.get(dest).contains(method)) {
                val result = inheritedMethods.get(dest)
                .removeIf[cmp.compare(it, method) == 0 && it.signature.modifiers.contains("abstract")]
                if (result) {
                    inheritedMethods.get(dest).add(method);
                }
            } else {
                inheritedMethods.get(dest).add(method);
            }
        }
    }

    def getMethodComparator() {
        return new Comparator<Method>() {
            override compare(Method arg1, Method arg2) {
                val md1 = arg1.signature.getHeader();
                val md2 = arg2.signature.getHeader();

                var int result = md1.name.compareTo(md2.name);
                if (result != 0) {
                    return result;
                }
                val lp1 = extractMethodParamsType(md1);
                val lp2 = extractMethodParamsType(md2);

                return String.join(", ", lp1).compareTo(String.join(", ", lp2));
            }
        }
    }

    def superType (String type1, String type2) {
        return if (!isCompatibleType(type1, type2)) type1 else type2;
    }

    def isCompatibleType(String type1, String type2) {
        switch (type1) {
            case "LongType"   : {
                return Arrays.asList("IntType", "LongType")
                .contains(type2);
            }
            case "FloatType"  : {
                return Arrays.asList("IntType", "LongType", "FloatType")
                .contains(type2);
            }
            case "DoubleType" : {
                return Arrays.asList("IntType", "LongType", "FloatType", "DoubleType")
                .contains(type2);
            }
            case "NullType": {
                return !Arrays.asList("IntType", "LongType", "FloatType", "DoubleType", "BooleanType")
                .contains(type2)
            }
            case type2: {
                return true;
            }
            default: {
                return false;
            }
        }
    }

    def evaluateType(String owner, Expression expr, List<Variable> scope) {
        if (expr instanceof CastExpression) {
            if (expr.types.isNullOrEmpty()) {
                var String exprClass = expr.expression.eClass.name;

                if (exprClass.equals("IntegerLiteral")) {
                    if ((expr.expression as IntegerLiteral).suffix != null) {
                        return "LongType";
                    } else {
                        return "IntType";
                    }
                } else if (exprClass.equals("FloatingLiteral")) {
                    val suffix = (expr.expression as FloatingIntLiteral).suffix;

                    if(suffix != null && (suffix.equals("d") || suffix.equals("D"))) {
                        return "DoubleType";
                    } else {
                        return "FloatType";
                    }
                } else if (exprClass.equals("BooleanLiteral")) {
                    return "BooleanType";
                } else if (exprClass.equals("NullLiteral")) {
                    return "NullType";
                } else if (exprClass.equals("StringLiteral")) {
                    return "String";
                } else if (exprClass.equals("ObjectLiteral")) {
                    return (expr.expression as ObjectLiteral).value;
                } else if (exprClass.equals("ClassInstanceCreationExpression")) {
                    return ((expr.expression as ClassInstanceCreationExpression).type as ObjectType).name;
                } else if (exprClass.equals("MethodInvocation")) {
                    return checkMethodInvacation(owner, expr.expression as MethodInvocation);
                } else {
                    //Expression out of spoce
                    return null;
                }
            } else {
                //Expression out of spoce
                return null;
            }
        } else if (expr instanceof LogicalExpression || expr instanceof TestingExpression) {
            //TODO(diegoadolfo): create check to validate expression members
            return "BooleanType";
        } else if (expr instanceof NumericExpression) {
            return checkNumericExpression(owner, expr, scope);
        } else {
            //Expression out of spoce
            return null;
        }
    }

    def deriveVariableTypeFromScope(String owner, String varName) {
        var String varType;

        for (Attribute variable : attributes.get(owner)) {
            if (variable.type instanceof ObjectType)
                varType = (variable.type as ObjectType).name
            else
                varType = variable.type.eClass.name;

            for (VariableDeclarator declaration  : variable.declarations) {
                if (varName.equals(declaration.facade.name)) {
                    return varType;
                }
            }
        }
        return null;
    }

    def deriveVariableTypeFromScope(String owner, String varName, List<Variable> scope) {
        var String varType;

        for (Variable variable : scope) {
             if (variable.type instanceof ObjectType)
                 varType = (variable.type as ObjectType).name
             else
                 varType = variable.type.eClass.name;

             for (VariableDeclarator declaration  : variable.declarations) {
                if (varName.equals(declaration.facade.name)) {
                    return varType;
                }
            }
        }
        return deriveVariableTypeFromScope(owner, varName);
    }
}