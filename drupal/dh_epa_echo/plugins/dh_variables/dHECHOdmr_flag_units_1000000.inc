<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
  'variable' =>  array(
    'varname' => 'dmr_flag_units_1000000', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'VFlag that indicates DMR entries that exceed 1,000,000*the median discharge value for the entire outfall',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'dmr_flag_units_1000000',
    'datatype' => 'flag',
    'varcode' => 'UnitFlagMGD',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'dmr_flag_units_1000000',
  ),
);

?>