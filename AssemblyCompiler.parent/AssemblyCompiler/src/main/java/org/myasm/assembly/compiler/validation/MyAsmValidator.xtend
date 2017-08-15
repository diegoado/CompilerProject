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
import org.myasm.assembly.compiler.myAsm.CastExpression
import org.myasm.assembly.compiler.myAsm.ClassDeclaration
import org.myasm.assembly.compiler.myAsm.ClassInstanceCreationExpression
import org.myasm.assembly.compiler.myAsm.CompilationUnit
import org.myasm.assembly.compiler.myAsm.Expression
import org.myasm.assembly.compiler.myAsm.FormalParameter
import org.myasm.assembly.compiler.myAsm.FloatingIntLiteral
import org.myasm.assembly.compiler.myAsm.InterfaceDeclaration
import org.myasm.assembly.compiler.myAsm.IntegerLiteral
import org.myasm.assembly.compiler.myAsm.Method
import org.myasm.assembly.compiler.myAsm.MethodDeclarator
import org.myasm.assembly.compiler.myAsm.MethodInvocation
import org.myasm.assembly.compiler.myAsm.MyAsmPackage
import org.myasm.assembly.compiler.myAsm.NumericExpression
import org.myasm.assembly.compiler.myAsm.ObjectType
import org.myasm.assembly.compiler.myAsm.VariableDeclarator
import org.myasm.assembly.compiler.myAsm.TypeDeclaration
import org.myasm.assembly.compiler.myAsm.LogicalExpression
import org.myasm.assembly.compiler.myAsm.TestingExpression

class MyAsmValidator extends AbstractMyAsmValidator {

    private List<String> classes;
    private List<String> interfaces;

    private Map<String, Set<String>> extended;
    private Map<String, Set<String>> implemented;

    private Map<String, List<Method>>    methods;
    private Map<String, List<Attribute>> attributes;

    private List<String> finalCls;
    private Map<String, Set<String>> attributesN;
    
    @Check
    def programMapper(CompilationUnit program) {
        classes = new ArrayList<String>();
        interfaces = new ArrayList<String>();

        extended = new HashMap<String, Set<String>>();
        implemented = new HashMap<String, Set<String>>();

        methods = new HashMap<String, List<Method>>();
        attributes = new HashMap<String, List<Attribute>>();

        finalCls = new ArrayList<String>();
        attributesN = new HashMap<String, Set<String>>();

        addStringType();
        for (TypeDeclaration declaration : program.getDeclarations()) {
            if (declaration instanceof ClassDeclaration) {
                classes.add(declaration.name);
                implemented.put(declaration.name, new HashSet<String>());
            } else if (declaration instanceof InterfaceDeclaration) {
                interfaces.add(declaration.name);
            }
            methods   .put(declaration.name, new ArrayList<Method>());
            attributes.put(declaration.name, new ArrayList<Attribute>());

            extended   .put(declaration.name, new HashSet<String>());
            attributesN.put(declaration.name, new HashSet<String>());
        }
    }

    @Check
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
    }

    @Check
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
            }
        }
    }

    @Check
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
                }
            }
        }
    }

    @Check
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
    }

    @Check
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
                }
            }
        }
    }

    @Check
    def checkAttributeDeclaration(TypeDeclaration owner) {
        var String type;

        for (EObject declaration : owner.body.getDeclarations()) {
            if (declaration instanceof Attribute) {
                for (VariableDeclarator variable : declaration.getDeclarations()) {
                    if (attributesN.get(owner.name).contains(variable.facade.name)) {
                        error("Class attribute " + variable.facade.name + " already declared.",
                        variable,
                        MyAsmPackage.Literals.VARIABLE_DECLARATOR__FACADE);
                    }
                    if (declaration.type instanceof ObjectType) {
                        type = (declaration.type as ObjectType).name;

                        if (attributes.get(type) == null) {
                            error("Attribute type " + type + " unreachable.",
                            declaration,
                            MyAsmPackage.Literals.ATTRIBUTE__TYPE);
                        }
                    } else {
                        type = declaration.type.eClass.name;
                    }
                    checkAttributeType(owner.name, declaration, type);

                    attributes .get(owner.name).add(declaration);
                    attributesN.get(owner.name).add(variable.facade.name)
                }
            }
        }
    }

    def checkAttributeType(String owner, Attribute attribute, String expectedType) {
        for (VariableDeclarator variable : attribute.declarations) {
            if (variable.definition instanceof Expression) {
                val result = isCompatibleType(owner, variable.definition as Expression, expectedType) as Boolean;
                if (result != null && !result) {
                    error("Fail to derive a compatible type to " + expectedType + ", return type is not compatible.",
                    variable,
                    MyAsmPackage.Literals.VARIABLE_DECLARATOR__DEFINITION);
                } else if (result == null) {
                    warning("Type expression validation is not available.",
                    variable,
                    MyAsmPackage.Literals.VARIABLE_DECLARATOR__DEFINITION);
                }
            } else if (variable.definition instanceof ArrayInitializer) {
            }
        }
    }

    @Check
    def checkMethodDeclaration(TypeDeclaration declaration) {
        for (EObject definition : declaration.body.getDeclarations()) {
            if (definition instanceof Method) {
                val type = definition.signature.getType();
                val modifiers = definition.signature.getModifiers();

                //TODO(diegoadolfo): check method modifiers
                if (definition.body != null && modifiers.contains("abstract")) {
                    error("An abstract method must have an empty body.",
                    definition.signature,
                    MyAsmPackage.Literals.METHOD_HEADER__MODIFIERS);
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

                if (!(declaration instanceof InterfaceDeclaration) && !modifiers.contains("abstract")) {
                    checkMethodBody(declaration.name, definition);
                }
                methods.get(declaration.name).add(definition);
            }
        }
    }

    @Check
    //TODO(diegoadolfo): this check validate only immediate inheritance
    def checkClassInheritance(ClassDeclaration clazz) {
        if (clazz.getExtends() != null && finalCls .contains(clazz.getExtends().name)) {
            error("The class " + clazz.getExtends().name + " is final, it cannot be inherited.",
            clazz.getExtends(),
            MyAsmPackage.Literals.SUPER_CLASS__NAME);
        }

        val classMethods = getClassMethods(clazz);
        var Set<Method> inheritedMethods = new TreeSet<Method>(getMethodComparator());

        if ((clazz.modifiers != null && !clazz.modifiers.contains("abstract")) || clazz.modifiers == null) {

            for (String superClass : extended.get(clazz.name)) {
                val abstractMethods = methods.get(superClass)
                .filter[it.signature.modifiers != null && it.signature.modifiers.contains("abstract")];

                inheritedMethods.addAll(abstractMethods);
            }
            if (!classMethods.containsAll(inheritedMethods)) {
                error("The class " + clazz.name + " must implement all superclass abstract methods.",
                clazz.getExtends(),
                MyAsmPackage.Literals.SUPER_CLASS__NAME);
            }
            inheritedMethods.clear();

            for (String interfacce : implemented.get(clazz.name)) {
                inheritedMethods.addAll(methods.get(interfacce));
            }

            if (!classMethods.containsAll(inheritedMethods)) {
                error("The class " + clazz.name + " must implement all interfaces methods.",
                clazz.getImplements(),
                MyAsmPackage.Literals.INTERFACE_LIST__INTERFACES);
            }
        }
    }

    def checkMethodHeader(String clazz, MethodDeclarator header) {
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
        val methods = methods.get(clazz)
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
    }

    def checkMethodInvacation(String owner, MethodInvocation method, String expectedType) {
        return null;
    }

    def checkNumericExpression(String owner, NumericExpression expr, String expectedType) {
        return null;
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

    def isCompatibleType(String type1, String type2) {
        switch (type1) {
            case "IntType"    : {
                return type2.equals("IntegerLiteral");
            }
            case "LongType"   : {
                return Arrays.asList("IntegerLiteral", "LongLiteral")
                .contains(type2);
            }
            case "FloatType"  : {
                return Arrays.asList("IntegerLiteral", "LongLiteral", "FloatingLiteral")
                .contains(type2);
            }
            case "DoubleType" : {
                return Arrays.asList("IntegerLiteral", "LongLiteral", "FloatingLiteral", "DoubleType")
                .contains(type2);
            }
            case "BooleanType": {
                return type2.equals("BooleanLiteral");
            }
            case "String"     : {
                return type2.equals("StringLiteral");
            }
            case type2: {
                return true;
            }
            default: {
                return false;
            }
        }
    }

    def isCompatibleType(String owner, Expression expr, String expectedType) {
        if (expr instanceof CastExpression) {
            if (expr.types.isNullOrEmpty()) {
                var String currType = expr.expression.eClass.name;

                if (currType.equals("IntegerLiteral") &&
                        (expr.expression as IntegerLiteral).suffix != null) {
                    currType = "LongLiteral";
                } else if (currType.equals("FloatingLiteral")) {
                    val suffix = (expr.expression as FloatingIntLiteral).suffix;

                    if (suffix  != null && (suffix.equals("d") || suffix.equals("D"))) {
                        currType = "DoubleLiteral";
                    }
                } else if (currType.equals("ClassInstanceCreationExpression")) {
                    currType = ((expr.expression as ClassInstanceCreationExpression).type as ObjectType).name;
                } else if (currType.equals("ArrayCreationExpression")) {
                    return null;
                } else if (currType.equals("MethodInvocation")) {
                    return checkMethodInvacation (owner, expr.expression as MethodInvocation, expectedType);
                }
                return new Boolean(isCompatibleType(expectedType, currType));
            } else {
                //Expression out of spoce
                return null;
            }
        } else if (expr instanceof LogicalExpression || expr instanceof TestingExpression) {
            if (!expectedType.equals("BooleanType")) {
                return new Boolean(false);
            } else {
                return new Boolean(true );
            }
        } else if (expr instanceof NumericExpression) {
            return checkNumericExpression(owner, expr, expectedType);
        } else {
            //Expression out of spoce
            return null;
        }
    }
}

