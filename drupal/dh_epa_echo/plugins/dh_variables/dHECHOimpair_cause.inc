<?php
// this is a pluginless plugin (only a variable def)

$plugin = array(
  'variable' =>  array(
    'varname' => 'impair_cause', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'The general categories of pollutants or stressors that are causing impairment in the assessed waterbody, as maintained in the ATTAINS database. The complete list and descriptions of impairment groups are provided in the National Causes of Impairment table',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'impair_cause',
    'datatype' => 'categories',
    'varcode' => 'AttainsCauseGroups',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'impair_cause',
  ),
);

?>