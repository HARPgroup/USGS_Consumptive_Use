<?php

$plugin = array(
  'label' => t('dmr_flag_desflow'), // choose a unique, descriptive name here
  'handler' =>  array(
    'class' => 'dHVariablePluginDefault', // the function that customizes behavior dHVariablePluginDefault is default and provides customization based only on the variable definition data.
    'file' => 'dh.display.inc', // the file that holds the plugin class code.  dh.display.inc has code for dHVariablePluginDefault
  ),
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
    'plugin' => 'dHECHOdmr_flag_desflow',
    'options' => '',
    'varabbrev' => 'dmr_flag_desflow',
  ),
);

?>