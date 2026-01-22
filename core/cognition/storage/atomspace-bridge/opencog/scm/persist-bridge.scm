(define-module (opencog persist-bridge))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog bridge-config))
(load-extension
	(string-append opencog-ext-path-persist-bridge-types "libpersist-bridge-types")
	"persist_bridge_types_init")
(load-from-path "opencog/persist/bridge-types/persist_bridge_types.scm")
(load-extension (string-append opencog-ext-path-persist-bridge "libpersist-bridge") "opencog_persist_bridge_init")
(export cog-bridge-load-tables cog-bridge-load-rows)
(set-procedure-property! cog-bridge-load-tables 'documentation
"
  cog-bridge-load-tables STORAGE - Load table definitions
  Optionally specify the SQL schema name from which to load the tables.
  (not implemeneted)
")
(set-procedure-property! cog-bridge-load-rows 'documentation
"
  cog-bridge-load-rows STORAGE TABLE COLUMN ITEM - Load rows containing ITEM
  The ITEM is assumed to be some entry located in COLUMN in TABLE.
  This returns a list of rows that match
  If ITEM is a PRIMARY KEY, there will be at most one row.
  Example:
    (cog-bridge-load-rows
        (BridgeStorage \"postgres:///flybase\")
        (Predicate \"genotype\")
        (Variable \"genotype_id\")
        (Number 362100))
")