// list-classes PROJECT_DIR OUTPUT_FILE
//
// Start the Godot that godot-sys built (headless, in the given project) and
// write the names of the classes it has (ClassDB.get_class_list), one per
// line. godot-sys's Setup.hs builds and runs this after building Godot, so
// that its dependents know which classes the build options left in.

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "libgodot.h"

static GDExtensionInterfaceGetProcAddress get_proc_address;

static void on_level(void *userdata, GDExtensionInitializationLevel level) {
	(void)userdata;
	(void)level;
}

static GDExtensionBool init(GDExtensionInterfaceGetProcAddress gpa, GDExtensionClassLibraryPtr library,
		GDExtensionInitialization *r_init) {
	(void)library;
	get_proc_address = gpa;
	r_init->minimum_initialization_level = GDEXTENSION_INITIALIZATION_CORE;
	r_init->userdata = NULL;
	r_init->initialize = on_level;
	r_init->deinitialize = on_level;
	return 1;
}

// Storage for a Variant, String or StringName (at most 24 bytes).
typedef struct {
	uint64_t data[4];
} Opaque;

static GDExtensionInterfaceVariantCall variant_call;
static GDExtensionInterfaceStringNameNewWithLatin1Chars string_name_new;
static GDExtensionInterfaceVariantDestroy variant_destroy;

static void *proc(const char *name) {
	void *p = (void *)get_proc_address(name);
	if (!p) {
		fprintf(stderr, "list-classes: Godot has no interface function %s\n", name);
		exit(1);
	}
	return p;
}

// Call a method without arguments on a Variant.
static Opaque call(Opaque *self, const char *method) {
	Opaque name, result;
	GDExtensionCallError error;
	string_name_new(&name, method, 1);
	variant_call(self, &name, NULL, 0, &result, &error);
	if (error.error != GDEXTENSION_CALL_OK) {
		fprintf(stderr, "list-classes: calling %s failed (error %d)\n", method, (int)error.error);
		exit(1);
	}
	return result;
}

int main(int argc, char **argv) {
	if (argc != 3) {
		fprintf(stderr, "usage: list-classes PROJECT_DIR OUTPUT_FILE\n");
		return 2;
	}
	char *godot_argv[] = { "list-classes", "--headless", "--path", argv[1], NULL };
	GDExtensionObjectPtr instance = libgodot_create_godot_instance(4, godot_argv, init);
	if (!instance || !get_proc_address) {
		fprintf(stderr, "list-classes: could not create a Godot instance\n");
		return 1;
	}

	variant_call = proc("variant_call");
	string_name_new = proc("string_name_new_with_latin1_chars");
	variant_destroy = proc("variant_destroy");
	GDExtensionInterfaceGetVariantFromTypeConstructor from_type = proc("get_variant_from_type_constructor");
	GDExtensionInterfaceGetVariantToTypeConstructor to_type = proc("get_variant_to_type_constructor");
	GDExtensionInterfaceGlobalGetSingleton get_singleton = proc("global_get_singleton");
	GDExtensionInterfaceVariantGetIndexed get_indexed = proc("variant_get_indexed");
	GDExtensionInterfaceStringToUtf8Chars to_utf8 = proc("string_to_utf8_chars");
	GDExtensionInterfaceVariantGetPtrDestructor get_destructor = proc("variant_get_ptr_destructor");
	GDExtensionVariantFromTypeConstructorFunc object_to_variant = from_type(GDEXTENSION_VARIANT_TYPE_OBJECT);
	GDExtensionTypeFromVariantConstructorFunc variant_to_int = to_type(GDEXTENSION_VARIANT_TYPE_INT);
	GDExtensionTypeFromVariantConstructorFunc variant_to_string = to_type(GDEXTENSION_VARIANT_TYPE_STRING);
	GDExtensionPtrDestructor destroy_string = get_destructor(GDEXTENSION_VARIANT_TYPE_STRING);

	// The scene classes are only registered once the instance is started.
	Opaque instance_variant;
	object_to_variant(&instance_variant, &instance);
	Opaque started = call(&instance_variant, "start");
	variant_destroy(&started);

	Opaque class_db_name;
	string_name_new(&class_db_name, "ClassDB", 1);
	GDExtensionObjectPtr class_db = get_singleton(&class_db_name);
	if (!class_db) {
		fprintf(stderr, "list-classes: no ClassDB singleton\n");
		return 1;
	}
	Opaque class_db_variant;
	object_to_variant(&class_db_variant, &class_db);
	Opaque classes = call(&class_db_variant, "get_class_list");
	Opaque size_variant = call(&classes, "size");
	int64_t size = 0;
	variant_to_int(&size, &size_variant);

	FILE *out = fopen(argv[2], "w");
	if (!out) {
		perror(argv[2]);
		return 1;
	}
	for (int64_t i = 0; i < size; i++) {
		Opaque item, string;
		GDExtensionBool valid, oob;
		get_indexed(&classes, i, &item, &valid, &oob);
		if (!valid || oob) {
			fprintf(stderr, "list-classes: cannot read class %ld\n", (long)i);
			return 1;
		}
		variant_to_string(&string, &item);
		char name[256];
		GDExtensionInt n = to_utf8(&string, name, sizeof name - 1);
		name[n] = '\0';
		fprintf(out, "%s\n", name);
		destroy_string(&string);
		variant_destroy(&item);
	}
	if (fclose(out) != 0) {
		perror(argv[2]);
		return 1;
	}

	variant_destroy(&classes);
	variant_destroy(&size_variant);
	libgodot_destroy_godot_instance(instance);
	return 0;
}
