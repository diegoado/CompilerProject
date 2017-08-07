package org.myasm.assembly.compiler.validation

import java.util.*;

import org.eclipse.xtext.validation.Check
import org.myasm.assembly.compiler.myAsm.Variable
import org.myasm.assembly.compiler.myAsm.CompilationUnit
import org.myasm.assembly.compiler.myAsm.Method
import java.util.HashMap
import java.util.List
import org.myasm.assembly.compiler.myAsm.Attribute
import org.myasm.assembly.compiler.myAsm.TypeDeclaration
import org.myasm.assembly.compiler.myAsm.ClassDeclaration
import org.myasm.assembly.compiler.myAsm.InterfaceDeclaration

//import org.myasm.assembly.compiler.myAsm.VariableDeclarator
//import org.myasm.assembly.compiler.myAsm.ConditionalAndExpression
//import org.eclipse.emf.ecore.EObject
//import org.myasm.assembly.compiler.myAsm.VariableInitializer
//import org.myasm.assembly.compiler.myAsm.ConditionalExpression
//import org.myasm.assembly.compiler.myAsm.ConditionalOrExpression
//import org.myasm.assembly.compiler.myAsm.Literal
//import org.myasm.assembly.compiler.myAsm.Expression

/**
 * This class contains custom validation rules. 
 *
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation
 */
class MyAsmValidator extends AbstractMyAsmValidator {

    private Map<String, List<Method>>    methods;
    private Map<String, List<Attribute>> attributes;

    @Check
    def programMapper(CompilationUnit program) {
        methods    = new HashMap<String, List<Method>>();
        attributes = new HashMap<String, List<Attribute>>();
        addStringType();

        for (TypeDeclaration declaration : program.getDeclarations()) {
            if (declaration instanceof ClassDeclaration) {
                attributesMapper(declaration);
                methodsMapper   (declaration);
            } else if (declaration instanceof InterfaceDeclaration) {

            }
        }
    }

    def methodsMapper   (ClassDeclaration clazz) {

    }

    def attributesMapper(ClassDeclaration clazz) {

    }

    @Check
    def checkVariableDeclarator(Variable variable) {
    }

    def addStringType() {
        methods.put("String", new ArrayList<Method>());
        attributes.put("String", new ArrayList<Attribute>());
    }
}
