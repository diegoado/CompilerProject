/*
 * generated by Xtext 2.9.2
 */
package org.myasm.assembly.compiler


/**
 * Initialization support for running Xtext languages without Equinox extension registry.
 */
class MyAsmStandaloneSetup extends MyAsmStandaloneSetupGenerated {

	def static void doSetup() {
		new MyAsmStandaloneSetup().createInjectorAndDoEMFRegistration()
	}
}
