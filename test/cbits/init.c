#include <libgodot.h>

static void noop_initialize(void *userdata, GDExtensionInitializationLevel level) {
	(void)userdata;
	(void)level;
}

/* Minimal GDExtension entry point for the host application. */
GDExtensionBool godot_sys_test_init(
		GDExtensionInterfaceGetProcAddress get_proc_address,
		GDExtensionClassLibraryPtr library,
		GDExtensionInitialization *r_initialization) {
	(void)library;
	if (get_proc_address == NULL) {
		return 0;
	}
	r_initialization->minimum_initialization_level = GDEXTENSION_INITIALIZATION_CORE;
	r_initialization->userdata = NULL;
	r_initialization->initialize = noop_initialize;
	r_initialization->deinitialize = noop_initialize;
	return 1;
}
