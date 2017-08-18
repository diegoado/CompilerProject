package org.myasm.assembly.compiler.generator

import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.generator.AbstractGenerator
import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.xtext.generator.IGeneratorContext
import org.myasm.assembly.compiler.myAsm.ClassDeclaration
import org.myasm.assembly.compiler.myAsm.VariableDeclarator
import org.myasm.assembly.compiler.myAsm.StringLiteral
import java.util.HashMap
import org.myasm.assembly.compiler.myAsm.Literal
import org.myasm.assembly.compiler.myAsm.Expression
import org.myasm.assembly.compiler.myAsm.NumericExpression
import org.myasm.assembly.compiler.myAsm.TestingExpression
import org.myasm.assembly.compiler.myAsm.CastExpression
import org.myasm.assembly.compiler.myAsm.IntegerLiteral
import org.myasm.assembly.compiler.myAsm.BooleanLiteral
import org.myasm.assembly.compiler.myAsm.ObjectLiteral
import org.myasm.assembly.compiler.myAsm.Method
import org.myasm.assembly.compiler.myAsm.DeclarationBody
import org.myasm.assembly.compiler.myAsm.Variable
import org.myasm.assembly.compiler.myAsm.Attribute
import org.eclipse.emf.ecore.EObject
import org.myasm.assembly.compiler.myAsm.AssignmentStatement
import org.myasm.assembly.compiler.services.MyAsmGrammarAccess.ExpressionNameElements
import org.myasm.assembly.compiler.myAsm.MethodInvocation
import org.myasm.assembly.compiler.myAsm.Statement
import org.myasm.assembly.compiler.myAsm.PrimaryExpression
import org.myasm.assembly.compiler.myAsm.FormalParameter
import java.util.List
import java.util.ArrayList
import org.myasm.assembly.compiler.myAsm.MethodHeader
import org.myasm.assembly.compiler.myAsm.VoidType

/**
 * Generates code from your model files on save.
 *
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#code-generation
 */
class MyAsmGenerator extends AbstractGenerator {
    private HashMap<String, List<String>> methods_parans;
    private HashMap<String, Boolean> methods_return;
    Resource resource;
    private int stringCounter = 0;

    override void doGenerate(Resource resource, IFileSystemAccess2 fsa, IGeneratorContext context) {
        this.resource = resource;
        for (e : resource.allContents.toIterable.filter(ClassDeclaration)) {
        	methods_parans = new HashMap<String, List<String>>();
        	methods_return = new HashMap<String, Boolean>();
            var String nomeDoArquivo = e.name + ".asm";
            fsa.generateFile(nomeDoArquivo, e.compile());
        }
    }

    def compile(ClassDeclaration classDeclaration) '''
		; Programa «classDeclaration.name»
		; Data
		«classDeclaration.compileAttribute()»
		; Codigo
		«classDeclaration.compileMethod()»
    	; Main
    	«//JAVA MAIN
   		 »
    	ret; Fim do programa
	'''
	def compileAttribute(ClassDeclaration classDeclaration) '''
 			«var DeclarationBody body  = classDeclaration.body as DeclarationBody»
 			«FOR EObject variable : body.declarations»
 				«IF variable instanceof Attribute»
 					«FOR VariableDeclarator variableDeclarator : variable.declarations»
 						«variableDeclarator.geraAssemblyVariableDeclarator»
 					«ENDFOR»
 				«ENDIF»
 			«ENDFOR»
 	'''
    def geraAssemblyVariableDeclarator(VariableDeclarator variable) {
        var String id = variable.facade.name;
        var Expression initializer = variable.definition as Expression;
        System.out.println("Initializer: " + initializer);
        var String linhas = String.valueOf(evaluateExpression(initializer));
        var String store = "ST " + id + ",R0";
        var String comentario = "; Atribuindo variavel "+id;
        return  comentario + "\n"+linhas + "\n" + store ;
    }
    def evaluateExpression(Expression expression) {
        evaluateExpression(expression,0);
    }
    def evaluateExpression(Expression expression, int counter) {
        if (expression instanceof NumericExpression) {
            var String op = expression.operator;
            var String leftExpression = evaluateExpression(expression.left,0) as String+"\n";
            var String rightExpression = evaluateExpression(expression.right,1) as String+"\n";
            var String operation = leftExpression + rightExpression + returnOperator(op)+ " R0,R0,R1";
            return operation
        }
        if (expression instanceof TestingExpression) {
            var String op = expression.operator;
            var Literal esquerda = ((expression.left as CastExpression).expression as Literal);
            var Literal direita = ((expression.right as CastExpression).expression as Literal)
            if (op !== null) {
                if (esquerda instanceof IntegerLiteral) {
                    return testaIntegerLiteral(esquerda, direita as IntegerLiteral, op);
                } else {
                    return testaBooleanLiteral(esquerda as BooleanLiteral, direita as BooleanLiteral, op);
                }
            } else {
                if (esquerda instanceof IntegerLiteral) {
                    return "LD R"+(counter)+"," + esquerda.value;
                } else if (esquerda instanceof BooleanLiteral) {
                    return "LD R"+(counter)+"," + b2s(esquerda.value);
                } else {
                    var String retorno = "__STRING_" + stringCounter + " DB " + "'" +
                            (esquerda as StringLiteral).value + "',0\n" +
                            "__STRING_" + stringCounter + " EQU $- __STRING_" +stringCounter+
                            "\nLD R0,__STRING_" + stringCounter;
                    stringCounter++;
                    return retorno;
                }
            }
        }
        if (expression instanceof CastExpression) {
        	if(expression.expression instanceof ObjectLiteral) {
        		var ObjectLiteral esquerda = expression.expression as ObjectLiteral;    		
                return "LD R"+(counter)+"," + esquerda.value;
        	}
        	else{
            var Literal esquerda = expression.expression as Literal;
            if (esquerda instanceof IntegerLiteral) {
                return "LD R"+(counter)+"," + esquerda.value;
            } else if (esquerda instanceof BooleanLiteral) {
                return "LD R"+(counter)+"," + b2s(esquerda.value);
            } else {
                var String retorno = "__STRING_" + stringCounter + " DB " + "'" +
                        (esquerda as StringLiteral).value + "',0\n" +
                        "__STRING_" + stringCounter + " EQU $- __STRING_" +stringCounter+
                        "\nLD R0,__STRING_" + stringCounter;
                stringCounter++;
                return retorno;
            }
			}
        }
    }
    def testaBooleanLiteral(BooleanLiteral literal, BooleanLiteral literal2, String op) {
        if (op.equals("==")) {
            return "LD R0,"+b2s(literal.value == literal2.value);
        } else if (op.equals("!=")) {
            return "LD R0,"+b2s(literal.value != literal2.value);
        }
    }
    def testaIntegerLiteral(IntegerLiteral literal, IntegerLiteral literal2, String op) {
        if (op.equals(">")) {
            return "LD R0,"+b2s(literal.value > literal2.value);
        } else if (op.equals(">=")) {
            return "LD R0,"+b2s(literal.value >= literal2.value);
        } else if (op.equals("<")) {
            return "LD R0,"+b2s(literal.value < literal2.value);
        } else if (op.equals("<=")) {
            return "LD R0,"+b2s(literal.value <= literal2.value);
        }
    }
    def returnOperator(String op) {
        if (op.equals("+")) {
            return "ADD";
        }
        if (op.equals("-")) {
            return "SUB";
        }
        if (op.equals("*")) {
            return "MUL";
        }
        if (op.equals("+")) {
            return "DIV";
        }
        if (op.equals("==")) {
            return "BEQ";
        }
        if (op.equals("!=")) {
            return "BNE";
        }
        if (op.equals(">")) {
            return "BGTZ";
        }
        if (op.equals(">=")) {
            return "BGEZ";
        }
        if (op.equals("<")) {
            return "BLTZ";
        }
        if (op.equals("<=")) {
            return "BLEZ";
        }
    }
    def b2s(boolean b) {
        if (b == true) {
            return 1;
        }
        if (b == false) {
            return 0;
        }
    }
    def b2s(String b) {
        if (b == "true") {
            return 1;
        }
        if (b == "false") {
            return 0;
        }
    }
    
    //Method
    
    def compileMethod(ClassDeclaration classDeclaration) '''
    	«var DeclarationBody body  = classDeclaration.body as DeclarationBody»
    		«FOR EObject element : body.declarations»
    		    	«IF element instanceof Method»
    		    		«element.methodMap»
    		    	«ENDIF»
    		 «ENDFOR»
    		«FOR EObject element : body.declarations»
    			«IF element instanceof Method»
    				«element.compileMethodVariable»
    			«ENDIF»
    		«ENDFOR»
    '''
    
    def compileMethodVariable(Method method)'''
    	;Procedure «method.signature.header.name»
    	«method.signature.header.name»:
    	«var DeclarationBody methodBody = method.body as DeclarationBody»
    	«FOR EObject variable : methodBody.declarations»
    		«IF variable instanceof CastExpression»
    			«evaluateMethodInvocation(variable.expression as MethodInvocation)»
    		«ENDIF»
    	    «IF variable instanceof Variable»
    	    	«FOR VariableDeclarator variableDeclarator : variable.declarations»
    	    		«variableDeclarator.geraAssemblyVariableDeclarator»
    	    	«ENDFOR»
    	    «ENDIF»
    	«ENDFOR»
    '''
     
    def compileAssigment(AssignmentStatement assignmentStatement )'''
     	«var Expression expression = assignmentStatement.right as Expression»
     	«expression.evaluateExpression»
    '''
    def evaluateMethodInvocation(MethodInvocation invocation){
 		var String ultima_linha= "call "+ invocation.name + ":"; 
 		var List<Expression> params = invocation.params.declarations;
 		var String return_method =  "";
 		if(methods_return.get(invocation.name)){
 			return_method += "\nST " + "return_"+invocation.name + ",R0";
 		}
 		var String code = "";
 		var int i = 0;
 		for(Expression exp : params){
 			code+= evaluateExpression(exp);
 			code+= "\nST " + methods_parans.get(invocation.name).get(i) + ",R0\n";
 			i++;
 		}
 		return ";Chamando " + invocation.name + "\n"+ code + ultima_linha;//+ return_method; 
  	}
     
    def methodMap(Method method){
    	var ArrayList<String> list_parans = new ArrayList<String>();
    	for( FormalParameter parameter : method.signature.header.params){
    		list_parans.add(parameter.variable.name);
    	}
    	methods_parans.put(method.signature.header.name,list_parans);
    	var MethodHeader header = method.signature.header as MethodHeader;
    	if(header.type instanceof VoidType){
    		methods_return.put(method.signature.header.name,false);
    	}else 
    		methods_return.put(method.signature.header.name,true);
    }
     
     
}
