package org.myasm.assembly.compiler.validation

import java.util.*;

import org.eclipse.xtext.validation.Check
import org.myasm.assembly.compiler.myAsm.Variable
import org.myasm.assembly.compiler.myAsm.CompilationUnit
import org.myasm.assembly.compiler.myAsm.Method
import java.util.HashMap
import java.util.List

import org.myasm.assembly.compiler.myAsm.MyAsmPackage
import org.myasm.assembly.compiler.myAsm.Attribute
import org.myasm.assembly.compiler.myAsm.TypeDeclaration
import org.myasm.assembly.compiler.myAsm.ClassDeclaration
import org.myasm.assembly.compiler.myAsm.InterfaceDeclaration
import java.util.Collections
import org.myasm.assembly.compiler.myAsm.SuperClass

//import org.myasm.assembly.compiler.myAsm.VariableDeclarator
//import org.myasm.assembly.compiler.myAsm.ConditionalAndExpression
//import org.eclipse.emf.ecore.EObject
//import org.myasm.assembly.compiler.myAsm.VariableInitializer
//import org.myasm.assembly.compiler.myAsm.ConditionalExpression
//import org.myasm.assembly.compiler.myAsm.ConditionalOrExpression
//import org.myasm.assembly.compiler.myAsm.Literal
//import org.myasm.assembly.compiler.myAsm.Expression

class MyAsmValidator extends AbstractMyAsmValidator {

    private List<String> classes;
    private List<String> interfaces;

    private Map<String, List<String>> typeDeclarationExtends;
    private Map<String, List<String>> typeDeclarationImplements;

    private Map<String, List<Method>>    methods;
    private Map<String, List<Attribute>> attributes;
    
    @Check
    def programMapper(CompilationUnit program) {
        classes    = new ArrayList<String>();
        interfaces = new ArrayList<String>();

        typeDeclarationExtends    = new HashMap<String, List<String>>();
        typeDeclarationImplements = new HashMap<String, List<String>>();

        methods    = new HashMap<String, List<Method>>();
        attributes = new HashMap<String, List<Attribute>>();

        addStringType();

        for (TypeDeclaration declaration : program.getDeclarations()) {
            if (declaration instanceof ClassDeclaration) {
                classes.add(declaration.name);
                typeDeclarationImplements.put(declaration.name, new ArrayList<String>());
            } else if (declaration instanceof InterfaceDeclaration) {
                interfaces.add(declaration.name);
            }
            methods   .put(declaration.name, new ArrayList<Method>());
            attributes.put(declaration.name, new ArrayList<Attribute>());

            typeDeclarationExtends.put(declaration.name, new ArrayList<String>());
        }
    }

    @Check
    def checkClass(ClassDeclaration clazz) {
        if (Collections.frequency(classes, clazz.name) > 1) {
            error("The class " + clazz.name + " already declared.", clazz,
            MyAsmPackage.Literals.TYPE_DECLARATION__NAME);
        }
        var List<String> modifiers = new ArrayList<String>();
        if (clazz.modifiers != null) {
            for (String modifier : clazz.modifiers) {
                modifiers.add(modifier);
            }
        }
        if (modifiers.contains("abstract") && modifiers.contains("final")) {
            error("Only one of the modifiers can be used: abstract or final.", clazz,
            MyAsmPackage.Literals.TYPE_DECLARATION__MODIFIERS);
        } else if (Collections.frequency(modifiers, "abstract") > 1 || Collections.frequency(modifiers, "final") > 1) {
            error("Only one instance of the final or abstract modifier is allowed.", clazz,
            MyAsmPackage.Literals.TYPE_DECLARATION__MODIFIERS);
        }
    }

    @Check
    def checkSuperClass(ClassDeclaration clazz) {
        if (clazz.getExtends() != null) {
            var String superClassName = clazz.getExtends().name;

            if (interfaces.contains(superClassName)) {
                error(superClassName + " is a interface.", clazz,
                MyAsmPackage.Literals.CLASS_DECLARATION__EXTENDS);
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
