<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
'variable' =>  array(
    'varname' => 'last_inspect', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'Indicates the number of days since the most recent inspection of the facility. Inspections listed within the last five years are included',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'last_inspect',
    'datatype' => 'date',
    'varcode' => 'LastInspection',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'day',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'last_inspect',
  ),
);

?>