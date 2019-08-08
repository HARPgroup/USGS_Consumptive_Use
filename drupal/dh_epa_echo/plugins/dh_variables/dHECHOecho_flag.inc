<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
  'variable' =>  array(
    'varname' => 'echo_flag', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'Violation Code assigned by the EPAs Enforcement and Compliance History Online System that describes the type of effluent or monitoring/reporting violation of the (Discharge Monitoring Report) DMR entry.',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'echo_flag',
    'datatype' => 'flag',
    'varcode' => 'ViolationCode',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'echo_flag',
  ),
);

?>