<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    if($_M['form']['pageset']){
        $pullpage_id = explode("<m",$ui['pullpage_id']);
        $pullpage_id = $pullpage_id[0];
        
    }else{
        $pullpage_id = $ui['pullpage_id'];
    }
?>
<div data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>"  data-fun="$uicss_<?php echo $pullpage_id;?>" class="$uicss section met-index-product met-index-body met-index-newproduct     <?php if($ui['bg_type']){ ?>bgcolor<?php }else{ ?>bgpic<?php } ?>     <?php if($ui['heightif']){ ?>heightbgd<?php }else{ ?>heightgd<?php } ?>" m-id='<?php echo $ui['mid'];?>'>
    <div class="wrapper">
        <div class="container">
            <div class="slideshow">
                <div class="slideshow-left ">
                    <?php
    $type=strtolower(trim('son'));
    $cid=$ui['id'];
    $column = load::sys_class('label', 'new')->get('column');

    unset($result);
    switch ($type) {
            case 'son':
                $result = $column->get_column_son($cid);
                break;
            case 'current':
                $result[0] = $column->get_column_id($cid);
                break;
            case 'head':
                $result = $column->get_column_head();
                break;
            case 'foot':
                $result = $column->get_column_foot();
                break;
            default:
                $result[0] = $column->get_column_id($cid);
                break;
        }
    $sub = count($result);
    foreach($result as $index=>$m):
        $hides = 1;
        $hide = explode("|",$hides);
        $m['_index']= $index;
        if($data['classnow']==$m['id'] || $data['class1']==$m['id'] || $data['class2']==$m['id']){
            $m['class']="";
        }else{
            $m['class'] = '';
        }
        if(in_array($m['name'],$hide)){
            unset($m['id']);
            unset($m['class']);
            $m['hide'] = $hide;
            $m['sub'] = 0;
        }


        if(substr(trim($m['icon']),0,1) == 'm' || substr(trim($m['icon']),0,1) == ''){
            $m['icon'] = 'icon fa-pencil-square-o '.$m['icon'];
        }
        $m['urlnew'] = $m['new_windows'] ? "target='_blank'" :"target='_self'";
        $m['urlnew'] = $m['nofollow'] ? $m['urlnew']." rel='nofollow'" :$m['urlnew'];
        $m['_first']=$index==0 ? true:false;
        $m['_last']=$index==(count($result)-1)?true:false;
        $$m = $m;
?>
                            <?php if($m[_index]<1){ ?>
                            <div class="Lslide" style="background:<?php echo $m['namemark'];?>">
                                <div class="Lslide-content">
                                    <h2 class="invisible" data-plugin="appear" data-animate="slide-top" data-repeat="false"><?php echo $m['name'];?></h2>
                                    <p  class="invisible" data-plugin="appear" data-animate="slide-top" data-repeat="false"><?php echo $m['description'];?></p>
                                    <div class="button invisible" data-plugin="appear" data-animate="slide-bottom" data-repeat="false">
                                        <a     <?php if($ui['iflink']){ ?>href="<?php echo $m['url'];?>"<?php }else{ ?>href="<?php echo $ui['link'];?>"<?php } ?> title="<?php echo $m['name'];?>" <?php echo $m['urlnew'];?>     <?php if($m['nofollow']){ ?>rel="nofollow"<?php } ?>>
                                            <p><?php echo $ui['more'];?></p>
                                            <i class="fa fa-chevron-right" aria-hidden="true"></i>
                                        </a>
                                    </div>
                                </div>
                            </div>
                        <?php }else{ ?>
                            <div class="Lslide" style="background:<?php echo $m['namemark'];?>">
                                <div class="Lslide-content">
                                    <h2 class=""><?php echo $m['name'];?></h2>
                                    <p class=""><?php echo $m['description'];?></p>
                                    <div class="button ">
                                        <a     <?php if($ui['iflink']){ ?>href="<?php echo $m['url'];?>"<?php }else{ ?>href="<?php echo $ui['link'];?>"<?php } ?> title="<?php echo $m['name'];?>" <?php echo $m['urlnew'];?>     <?php if($m['nofollow']){ ?>rel="nofollow"<?php } ?>>
                                            <p><?php echo $ui['more'];?></p>
                                            <i class="fa fa-chevron-right" aria-hidden="true"></i>
                                        </a>
                                    </div>
                                </div>
                            </div>
                        <?php } ?>
                    <?php endforeach;?>
                </div>
            <div class="slideshow-right " >
                        <?php
            $sub = count($result);
            $num = 30;
            if(!is_array($result)){
                $result = explode('|',$result);
            }
            foreach ($result as $index => $val) {
                if($index >= $num){
                    break;
                }
                if($sub <=0){
                    continue;
                }
                if(is_array($val)){
                    $val['_index'] = $index;
                    $val['_first'] = $index == 0 ? true : false;
                    $val['_last']  = $index == (count($result)-1) ? true : false;
                    $val['sub']    = $sub;
                }

                $l = $val;
            ?>
                    <div class="Rslide" style="background:url(<?php echo $l['columnimg'];?>) no-repeat;background-size: cover;background-position: center;">
                    </div>
                <?php }?>
            </div>
            <div class="control">
                <div class="oncontrol control-top invisible" data-plugin="appear" data-animate="slide-left" data-repeat="false">
                    <i class="fa fa-circle-o" aria-hidden="true"></i>
                </div>
                <div class="oncontrol control-bottom invisible" data-plugin="appear" data-animate="slide-right" data-repeat="false">
                    <i class="fa fa-arrow-down" aria-hidden="true"></i>
                </div>
            </div>
            </div>
        </div>
    </div>
</div>