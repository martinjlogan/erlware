%% Global structures for using the fs_test_lib

%% State for a whole test
-record(state, {data_dir, priv_dir, otp_dir, releases}).

%% ****** README DONT SKIP THESE DIRECTIONS ******
%% State for a single release. If you add a member to this record you must 
%% update the fs_test_lib:fetch_release_value/2 function.
-record(release, {name, node_name, sasl_log, err_log, contact_node, config_file, script_and_boot_dir, long_node_name = [], death_treatment = permanent}).



