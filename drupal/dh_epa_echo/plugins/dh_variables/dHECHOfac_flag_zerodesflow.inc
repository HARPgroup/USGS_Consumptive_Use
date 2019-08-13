<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
'variable' =>  array(
    'varname' => 'fac_flag_zerodesflow', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'Flag which identifies facilities that report a design flow equal to 0 MGD.',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'fac_flag_zerodesflow',
    'datatype' => 'flag',
    'varcode' => 'ZeroDesignFlow',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'fac_flag_zerodesflow',
  ),
);

?>