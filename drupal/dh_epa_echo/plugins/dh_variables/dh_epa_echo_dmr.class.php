<?php
module_load_include('inc', 'dh', 'plugins/dh.display');
module_load_include('module', 'dh');

class dHECHODMRPeriodMGD extends dHVariablePluginDefault {
  var $stat = 'mean';
  var $varkey2sum = 'wlg';
  var $rep_varkey = 'wd_gpy';
  
  public function update(&$entity) {
    // update dopplegangers
    //dpm($entity, 'update');
    $this->updateLinked($entity);
    parent::update($entity);
  }
  
  public function insert(&$entity) {
    // update dopplegangers
    //dpm($entity, 'insert');
    $this->updateLinked($entity);
    parent::insert($entity);
  }
  
  public function updateLinked(&$entity) {
    // dissaggregate period mean into individual monthly rates
    $syear = date('Y', dh_handletimestamp($entity->tstime));
    $smon = date('m', dh_handletimestamp($entity->tstime));
    $eyear = date('Y', dh_handletimestamp($entity->tsendtime));
    $emon = date('m', dh_handletimestamp($entity->tsendtime));
    $thisyear = $syear;
    $thismon = $smon;
    $dopple = array(
      'featureid' => $entity->featureid,
      'varkey' => $this->rep_varkey,
      'entity_type' => $entity->entity_type,
      'tsvalue' => $entity->tsvalue
    );
    while ( ($thisyear <= $eyear) and ($thismon <= $emon) ) {
      $begin = dh_handletimestamp("$year-$thismon-01 00:00:00");
      $modays = date('t', $begin);
      $end = dh_handletimestamp("$year-$thismon-$modays 00:00:00");
      $dopple['tstime'] = $begin;
      $dopple['tsendtime'] = $end;
      $tid = dh_update_timeseries($dopple, 'tstime_singular');
      dpm($dopple,"Updating $thisyear - $thismon ");
      $thismon = ($thismon == 12) ? 1 : $thismon + 1;
    }
    parent::updateLinked($entity);
  }
}

?>