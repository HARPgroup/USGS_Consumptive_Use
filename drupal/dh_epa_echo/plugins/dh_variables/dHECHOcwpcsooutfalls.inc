<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
  'variable' =>  array(
    'varname' => 'cwp_cso_outfalls', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'The number of discharge outfalls at points prior to the treatment plant',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'cwp_cso_outfalls',
    'datatype' => 'numeric',
    'varcode' => 'CWPCsoOutfalls',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'CWPCsoOutfalls',
  ),
);

?>