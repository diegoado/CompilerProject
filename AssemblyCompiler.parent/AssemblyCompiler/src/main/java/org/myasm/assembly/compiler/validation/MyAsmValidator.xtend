package org.myasm.assembly.compiler.validation

import java.util.ArrayList
import java.util.Collections
import java.util.List
import java.util.Map
import java.util.HashMap
import java.util.HashSet
import java.util.Set

import org.eclipse.xtext.validation.Check
import org.eclipse.emf.ecore.EObject

import org.myasm.assembly.compiler.myAsm.Attribute
import org.myasm.assembly.compiler.myAsm.ClassDeclaration
import org.myasm.assembly.compiler.myAsm.CompilationUnit
import org.myasm.assembly.compiler.myAsm.Expression
import org.myasm.assembly.compiler.myAsm.InterfaceDeclaration
import org.myasm.assembly.compiler.myAsm.InterfaceList
import org.myasm.assembly.compiler.myAsm.Method
import org.myasm.assembly.compiler.myAsm.MyAsmPackage
import org.myasm.assembly.compiler.myAsm.ObjectType
import org.myasm.assembly.compiler.myAsm.VariableDeclarator
import org.myasm.assembly.compiler.myAsm.TypeDeclaration
import org.myasm.assembly.compiler.myAsm.ArrayInitializer

class MyAsmValidator extends AbstractMyAsmValidator {

    private List<String> classes;
    private List<String> interfaces;

    private Map<String, Set<String>> typeDeclarationExtends;
    private Map<String, Set<String>> typeDeclarationImplements;

    private Map<String, List<Method>>     methodsType;
    private Map<String, List<Attribute>>  attributesType;

    private Map<String, Set<String>> attrStr;
    
    @Check
    def programMapper(CompilationUnit program) {
        classes    = new ArrayList<String>();
        interfaces = new ArrayList<String>();

        typeDeclarationExtends    = new HashMap<String, Set<String>>();
        typeDeclarationImplements = new HashMap<String, Set<String>>();

        methodsType    = new HashMap<String, List<Method>>();
        attributesType = new HashMap<String, List<Attribute>>();

        addStringType();
        attrStr = new HashMap<String, Set<String>>();

        for (TypeDeclaration declaration : program.getDeclarations()) {
            if (declaration instanceof ClassDeclaration) {
                classes.add(declaration.name);
                typeDeclarationImplements.put(declaration.name, new HashSet<String>());
            } else if (declaration instanceof InterfaceDeclaration) {
                interfaces.add(declaration.name);
            }
            methodsType   .put(declaration.name, new ArrayList<Method>());
            attributesType.put(declaration.name, new ArrayList<Attribute>());

            typeDeclarationExtends.put(declaration.name, new HashSet<String>());

            attrStr.put(declaration.name, new HashSet<String>());
            checkAttributeDeclaration(declaration)
        }
    }

    @Check
    def checkClassDeclaration(ClassDeclaration declaration) {
        if (Collections.frequency(classes, declaration.name) > 1) {
            error("The class " + declaration.name + " already declared.",
            declaration, MyAsmPackage.Literals.TYPE_DECLARATION__NAME);
        }
        var List<String> modifiers = new ArrayList<String>();
        if (declaration.modifiers != null) {
            for (String modifier : declaration.modifiers) {
                modifiers.add(modifier);
            }
        }
        if (modifiers.contains("abstract") && modifiers.contains("final")) {
            error("Only one of the modifiers can be used: abstract or final.",
            declaration, MyAsmPackage.Literals.TYPE_DECLARATION__MODIFIERS);
        } else if (Collections.frequency(modifiers, "abstract") > 1 || Collections.frequency(modifiers, "final") > 1) {
            error("Only one instance of the final or abstract modifier is allowed.", declaration,
            MyAsmPackage.Literals.TYPE_DECLARATION__MODIFIERS);
        }
    }

    @Check
    def checkClassExtends(ClassDeclaration declaration) {
        if (declaration.getExtends() != null) {
            var String superClassName = declaration.getExtends().name;

            if (interfaces.contains(superClassName)) {
                error(superClassName + " is a interface.", declaration,
                MyAsmPackage.Literals.CLASS_DECLARATION__EXTENDS);
            } else if (!classes.contains(superClassName)) {
                error("The class " + superClassName + " not declared.",
                declaration, MyAsmPackage.Literals.CLASS_DECLARATION__EXTENDS);
            } else if (declaration.name.equals(superClassName)) {
                error("The class cannot extends itself.", declaration,
                MyAsmPackage.Literals.CLASS_DECLARATION__EXTENDS);
            } else {
                typeDeclarationExtends.get(declaration.name).add(superClassName);
            }
        }
    }

    @Check
    def checkClassImplements(ClassDeclaration declaration) {
        var InterfaceList interfaceList = declaration.getImplements();

        if (interfaceList != null) {
            for (String interfaceName : interfaceList.getInterfaces()) {
                if (classes.contains(interfaceName)) {
                    error(interfaceName + " is a class.", declaration,
                    MyAsmPackage.Literals.CLASS_DECLARATION__IMPLEMENTS);
                } else if (!interfaces.contains(interfaceName)) {
                    error("The interface " + interfaceName + " not declared.",
                    declaration, MyAsmPackage.Literals.CLASS_DECLARATION__IMPLEMENTS);
                } else if (typeDeclarationExtends.get(declaration.name).contains(interfaceName)) {
                    error("The class " + declaration.name + " already implements interface " + interfaceName + ".",
                    declaration, MyAsmPackage.Literals.CLASS_DECLARATION__IMPLEMENTS);
                } else {
                    typeDeclarationExtends.get(declaration.name).add(interfaceName);
                }
            }
        }
    }

    @Check
    def checkInterfaceDeclaration(InterfaceDeclaration declaration) {
        if (Collections.frequency(interfaces, declaration.name) > 1) {
            error("The interface " + declaration.name + " already declared.",
            declaration, MyAsmPackage.Literals.TYPE_DECLARATION__NAME);
        }
        var List<String> modifiers = new ArrayList<String>();
        if (declaration.modifiers != null) {
            for (String modifier : declaration.modifiers) {
                modifiers.add(modifier);
            }
        }
        if (Collections.frequency(modifiers, "abstract") > 1) {
            error("Only one instance abstract modifier is allowed.", declaration,
            MyAsmPackage.Literals.TYPE_DECLARATION__MODIFIERS);
        }
    }

    @Check
    def checkInterfaceExtends(InterfaceDeclaration declaration) {
        var InterfaceList interfaceList = declaration.getExtends()

        if (interfaceList != null) {
            for (String interfaceName : interfaceList.getInterfaces()) {
                if (classes.contains(interfaceName)) {
                    error(interfaceName + " is a class.", declaration,
                    MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else if (!interfaces.contains(interfaceName)) {
                    error("The interface " + interfaceName + " not declared.",
                    declaration, MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else if (declaration.name.equals(interfaceName)) {
                    error("The interface cannot extends itself.", declaration,
                    MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else if (typeDeclarationExtends.get(declaration.name).contains(interfaceName)) {
                    error("The interface " + declaration.name + " already extends interface " + interfaceName + ".",
                    declaration, MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else {
                    typeDeclarationExtends.get(declaration.name).add(interfaceName);
                }
            }
        }
    }

    def checkAttributeDeclaration(TypeDeclaration owner) {
        for (EObject eObject : owner.body.getDeclarations()) {
            if (eObject instanceof Attribute) {
                for (VariableDeclarator declaration : eObject.getDeclarations()) {
                    if (attrStr.get(owner.name).contains(declaration.facade.name)) {
                        error("Class attribute " + declaration.facade.name + " already declared.",
                        declaration, MyAsmPackage.Literals.VARIABLE_DECLARATOR__FACADE);
                    } else if (eObject.type instanceof ObjectType) {
                        var ObjectType type = eObject.type as ObjectType;

                        if (attributesType.get(type.name) == null) {
                            error("Attribute type " + type.name + " unreachable.", eObject,
                            MyAsmPackage.Literals.ATTRIBUTE__TYPE);
                        }

                    }
                    attrStr.get(owner.name).add(declaration.facade.name)
                }
                attributesType.get(owner.name).add(eObject);
            }
        }
    }

    @Check
    def checkAttributeType(Attribute attribute) {
        switch (attribute.type.eClass.name) {
            case "IntType"    : {
                checkAttributeType(attribute, "int");
            }
            case "LongType"   : {
                checkAttributeType(attribute, "int", "long");
            }
            case "FloatType"  : {
                checkAttributeType(attribute, "int", "long", "float");
            }
            case "DoubleType" : {
                checkAttributeType(attribute, "int", "long", "float", "double");
            }
            case "BooleanType": {
                checkAttributeType(attribute, "boolean");
            }
            case "ObjectType" : {
                var ObjectType type = attribute.type as ObjectType;
                checkAttributeType(attribute, type.name);
            }
            default: {

            }
        }

    }

    def checkAttributeType (Attribute attribute, String... types) {
        for (VariableDeclarator declaration : attribute.getDeclarations()) {
        }
    }

    def addStringType() {
        methodsType.put("String", new ArrayList<Method>());
        attributesType.put("String", new ArrayList<Attribute>());
    }
}
