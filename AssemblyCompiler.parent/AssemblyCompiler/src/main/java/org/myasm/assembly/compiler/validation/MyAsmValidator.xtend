package org.myasm.assembly.compiler.validation

import java.util.ArrayList
import java.util.Collections
import java.util.List
import java.util.Map
import java.util.HashMap
import java.util.HashSet
import java.util.Set

import org.eclipse.xtext.validation.Check
import org.myasm.assembly.compiler.myAsm.Variable
import org.myasm.assembly.compiler.myAsm.CompilationUnit
import org.myasm.assembly.compiler.myAsm.Method

import org.myasm.assembly.compiler.myAsm.MyAsmPackage
import org.myasm.assembly.compiler.myAsm.Attribute
import org.myasm.assembly.compiler.myAsm.TypeDeclaration
import org.myasm.assembly.compiler.myAsm.ClassDeclaration
import org.myasm.assembly.compiler.myAsm.InterfaceDeclaration
import org.myasm.assembly.compiler.myAsm.InterfaceList


class MyAsmValidator extends AbstractMyAsmValidator {

    private List<String> classes;
    private List<String> interfaces;

    private Map<String, Set<String>> typeDeclarationExtends;
    private Map<String, Set<String>> typeDeclarationImplements;

    private Map<String, List<Method>>    methods;
    private Map<String, List<Attribute>> attributes;
    
    @Check
    def programMapper(CompilationUnit program) {
        classes    = new ArrayList<String>();
        interfaces = new ArrayList<String>();

        typeDeclarationExtends    = new HashMap<String, Set<String>>();
        typeDeclarationImplements = new HashMap<String, Set<String>>();

        methods    = new HashMap<String, List<Method>>();
        attributes = new HashMap<String, List<Attribute>>();

        addStringType();

        for (TypeDeclaration declaration : program.getDeclarations()) {
            if (declaration instanceof ClassDeclaration) {
                classes.add(declaration.name);
                typeDeclarationImplements.put(declaration.name, new HashSet<String>());
            } else if (declaration instanceof InterfaceDeclaration) {
                interfaces.add(declaration.name);
            }
            methods   .put(declaration.name, new ArrayList<Method>());
            attributes.put(declaration.name, new ArrayList<Attribute>());

            typeDeclarationExtends.put(declaration.name, new HashSet<String>());
        }
    }

    @Check
    def checkClassDeclaration(ClassDeclaration declaration) {
        if (Collections.frequency(classes, declaration.name) > 1) {
            error("The class " + declaration.name + " already declared.", declaration,
            MyAsmPackage.Literals.TYPE_DECLARATION__NAME);
        }
        var List<String> modifiers = new ArrayList<String>();
        if (declaration.modifiers != null) {
            for (String modifier : declaration.modifiers) {
                modifiers.add(modifier);
            }
        }
        if (modifiers.contains("abstract") && modifiers.contains("final")) {
            error("Only one of the modifiers can be used: abstract or final.", declaration,
            MyAsmPackage.Literals.TYPE_DECLARATION__MODIFIERS);
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
                error("The class " + superClassName + " not declared.", declaration,
                MyAsmPackage.Literals.CLASS_DECLARATION__EXTENDS);
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
            for (String strInterface : interfaceList.getInterfaces()) {
                if (classes.contains(strInterface)) {
                    error(strInterface + " is a class.", declaration,
                    MyAsmPackage.Literals.CLASS_DECLARATION__IMPLEMENTS);
                } else if (!interfaces.contains(strInterface)) {
                    error("The interface " + strInterface + " not declared.", declaration,
                    MyAsmPackage.Literals.CLASS_DECLARATION__IMPLEMENTS);
                } else if (typeDeclarationExtends.get(declaration.name).contains(strInterface)) {
                    error("The class " + declaration.name + " already implements interface " + strInterface + ".",
                    declaration, MyAsmPackage.Literals.CLASS_DECLARATION__IMPLEMENTS);
                } else {
                    typeDeclarationExtends.get(declaration.name).add(strInterface);
                }
            }
        }
    }

    @Check
    def checkInterfaceExtends(InterfaceDeclaration declaration) {
        var InterfaceList interfaceList = declaration.getExtends()

        if (interfaceList != null) {
            for (String strInterface : interfaceList.getInterfaces()) {
                if (classes.contains(strInterface)) {
                    error(strInterface + " is a class.", declaration,
                    MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else if (!interfaces.contains(strInterface)) {
                    error("The interface " + strInterface + " not declared.", declaration,
                    MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else if (declaration.name.equals(strInterface)) {
                    error("The interface cannot extends itself.", declaration,
                    MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else if (typeDeclarationExtends.get(declaration.name).contains(strInterface)) {
                    error("The interface " + declaration.name + " already extends interface " + strInterface + ".",
                    declaration, MyAsmPackage.Literals.INTERFACE_DECLARATION__EXTENDS);
                } else {
                    typeDeclarationExtends.get(declaration.name).add(strInterface);
                }
            }
        }
    }

    @Check
    def checkVariableDeclarator(Variable variable) {
    }

    def addStringType() {
        methods.put("String", new ArrayList<Method>());
        attributes.put("String", new ArrayList<Attribute>());
    }
}
