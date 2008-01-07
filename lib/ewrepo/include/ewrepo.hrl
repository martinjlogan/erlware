-define(PACKAGE_NAME_REGEXP, "[a-z]+[a-zA-Z0-9_]*").
-define(PACKAGE_VSN_REGEXP, "[a-zA-Z0-9_]+([.-][a-zA-Z0-9_]+)*").
-define(PACKAGE_EXT_REGEXP, "(\.tar\.gz|\.tgz|\.epkg)").
-define(PACKAGE_NAME_AND_VSN_REGEXP, 
	lists:flatten(["^", ?PACKAGE_NAME_REGEXP, "-", ?PACKAGE_VSN_REGEXP, ?PACKAGE_EXT_REGEXP, "*$"])).
