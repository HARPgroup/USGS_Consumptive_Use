<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
  'variable' =>  array(
    'varname' => 'cooling', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'Power station cooling water outfall.',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'cooling',
    'datatype' => 'categories',
    'varcode' => 'vpdes_outfall_type',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'cooling',
  ),
);

?>