/*
 * generated by Xtext 2.9.2
 */
package org.myasm.assembly.compiler.idea.completion

import org.eclipse.xtext.idea.lang.AbstractXtextLanguage
import org.myasm.assembly.compiler.idea.lang.MyAsmLanguage

class MyAsmCompletionContributor extends AbstractMyAsmCompletionContributor {
	new() {
		this(MyAsmLanguage.INSTANCE)
	}
	
	new(AbstractXtextLanguage lang) {
		super(lang)
		//custom rules here
	}
}
