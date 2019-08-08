<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
 'variable' =>  array(
    'varname' => 'wb_gnis_name', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'The name of the waterbody from the Geographic Names Information System (GNIS) database in which the facility is permitted to discharge directly',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'wb_gnis_name',
    'datatype' => 'text',
    'varcode' => 'RadGnisName',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'wb_gnis_name',
  ),
);

?>