<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
  'variable' =>  array(
    'varname' => 'stormwater', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'Outfall that tracks stormwater discharge through municipal separate storm sewer systems (MS4)',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'stormwater',
    'datatype' => 'categories',
    'varcode' => 'vpdes_outfall_type',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'stormwater',
  ),
);

?>