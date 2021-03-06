<?php
module_load_include('inc', 'dh', 'plugins/dh.display');
module_load_include('module', 'dh');

class dHECHODMRPeriodMGD extends dHVariablePluginDefault {
  var $stat = 'mean';
  var $rep_varkey = 'dmr_mon_mgd';
  
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
    $begin = dh_handletimestamp("$thisyear-$thismon-01 00:00:00");
    $dopple = array(
      'featureid' => $entity->featureid,
      'varkey' => $this->rep_varkey,
      'entity_type' => $entity->entity_type,
      'tsvalue' => $entity->tsvalue
    );
    while ( $begin < $entity->tsendtime ) {
      $modays = date('t', $begin);
      $end = dh_handletimestamp("$thisyear-$thismon-$modays 00:00:00");
      $dopple['tstime'] = $begin;
      $dopple['tsendtime'] = $end;
      $tid = dh_update_timeseries($dopple, 'tstime_singular');
      //dpm($dopple,"Updating $thisyear - $thismon with tid = $tid");
      if ($thismon == 12) {
        $thisyear++;
        $thismon = 1;
      } else {
        $thismon++;
      }
      $begin = dh_handletimestamp("$thisyear-$thismon-01 00:00:00");
    }
    parent::updateLinked($entity);
  }
}

class dHECHODMRMonMGD extends dHVariablePluginDefault {
  var $stat = 'mean';
  var $rep_varkey = 'dmr_mon_mgm';
  
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
	$modays = date('t', $entity->tstime);
    $dopple = array(
      'featureid' => $entity->featureid,
      'varkey' => $this->rep_varkey,
      'entity_type' => $entity->entity_type,
	  'tstime' => $entity->tstime,
	  'tsendtime' => $entity->tsendtime,
      'tsvalue' => $entity->tsvalue*$modays
    );
    
    $tid = dh_update_timeseries($dopple, 'tstime_singular');

    parent::updateLinked($entity);
  }
}
 
class dHECHODMRMonMGM extends dHVariablePluginDefault {
  var $stat = 'sum';
  var $rep_varkey = 'dmr_ann_mgy';
  
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
    // push monthly total to annual
    $year = date('Y', dh_handletimestamp($entity->tstime));
    $begin = dh_handletimestamp("$year-01-01 00:00:00");
    $end = dh_handletimestamp("$year-12-31 00:00:00");
    $summary = dh_summarizeTimePeriod($entity->entity_type, $entity->featureid, $entity->varid, $begin, $end);
    if (!empty($summary)) {
      $summary['varkey'] = $this->rep_varkey;
      $summary['tsvalue'] = $summary['sum_value'];
      $tid = dh_update_timeseries($summary, 'tstime_singular');
      //dpm($summary, "Updated TID $tid $this->rep_varkey Annual $year-01-01 to $year-12-31 From Monthly " . date("Y-m-d", $entity->tstime));
    } else {
      dsm("dh_summarizeTimePeriod returned FALSE ");
    }
    parent::updateLinked($entity);
  }
}

 
class dHECHODMRAnnMGY extends dHVariablePluginDefault {
  var $stat = 'sum';
  var $rep_varkey = 'dmr_ann_mgd';
  
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
	$yeardays = date('z', $entity->tsendtime)+1;
    $dopple = array(
      'featureid' => $entity->featureid,
      'varkey' => $this->rep_varkey,
      'entity_type' => $entity->entity_type,
	  'tstime' => $entity->tstime,
	  'tsendtime' => $entity->tsendtime,
      'tsvalue' => $entity->tsvalue/$yeardays
    );
    
    $tid = dh_update_timeseries($dopple, 'tstime_singular');

	
    parent::updateLinked($entity);
  }
}
?>