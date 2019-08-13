<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
  'variable' =>  array(
    'varname' => 'design_flow', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'Largest volume of water that the facility is allowed to handle within 24 hours (MGD).',
    'vocabulary' => 'epa_echo',
    'varunits' => 'mgd',
    'varkey' => 'design_flow',
    'datatype' => 'numeric',
    'varcode' => 'DesignFlow',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'day',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'design_flow',
  ),
);

?>