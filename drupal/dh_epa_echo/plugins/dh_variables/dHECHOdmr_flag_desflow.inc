<?php

$plugin = array(
  'variable' =>  array(
    'varname' => 'dmr_flag_desflow', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'Flag that indicates DMR entries that exceed the facility’s reported design flow.',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'dmr_flag_desflow',
    'datatype' => 'flag',
    'varcode' => 'MEgreaterDF',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'dmr_flag_desflow',
  ),
);

?>