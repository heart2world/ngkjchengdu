<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    if($_M['form']['pageset']){
        $pullpage_id = explode("<m",$ui['pullpage_id']);
        $pullpage_id = $pullpage_id[0];
    }else{
        $pullpage_id = $ui['pullpage_id'];
    }
?>
<div data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>" data-fun="$uicss_<?php echo $pullpage_id;?>" class="$uicss abouts section met-index-body     <?php if($ui['bg_type']){ ?>bgcolor<?php }else{ ?>bgpic<?php } ?>" m-id='<?php echo $ui['mid'];?>'>
    <div class="">
        <div class="">
            <div class="hero-slides" m-id='<?php echo $ui['mid'];?>'>
                <div class="title_box">
                        <?php if($ui['title']){ ?>
                        <h3 class="invisible" data-plugin="appear" data-animate="slide-top" data-repeat="false">
                            <?php
    $type=strtolower(trim('current'));
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
                                <a href="<?php echo $m['url'];?>" title="<?php echo $ui['title'];?>" <?php echo $m['urlnew'];?>><?php echo $ui['title'];?></a>
                            <?php endforeach;?>
                        </h3>
                    <?php } ?>
                        <?php if($ui['desc']){ ?>
                        <p class="invisible" data-plugin="appear" data-animate="slide-bottom" data-repeat="false"><?php echo $ui['desc'];?></p>
                    <?php } ?>
                </div>
                <div class="swiper-container invisible" data-num="<?php echo $ui['list_nums'];?>" data-listnum="<?php echo $ui['list_num'];?>" data-plugin="appear" data-animate="slide-bottom" data-repeat="false">
                    <div class="swiper-wrapper grid">
                        <?php
    $cid=$ui['id'];

    $num = $ui['list_num'];
    $module = "";
    $type = $ui['list_type'];
    $order = 'no_order asc';
    $para = "";
    if(!$module){
        if(!$cid){
            $value = $m['classnow'];
        }else{
            $value = $cid;
        }
    }else{
        $value = $module;
    }

    $result = load::sys_class('label', 'new')->get('tag')->get_list($value, $num, $type, $order, $para);
    $sub = count($result);
    foreach($result as $index=>$v):
        $id = $v['id'];
        $v['sub'] = $sub;
        $v['_index']= $index;
        $v['_first']= $index==0 ? true:false;
        $v['_last']=$index==(count($result)-1)?true:false;
        $$v = $v;
?>
                            <div class="swiper-slide">
                                <figure class="effect-sarah">
                                    <img src="<?php echo thumb($v['imgurl'],$ui['img_w'],$ui['img_h']);?>" alt="<?php echo $v['title'];?>"/>
                                    <figcaption>
                                        <h4><?php echo $v['title'];?></h4>
                                        <p><?php echo $v['description'];?></p>
                                        <a href="<?php echo $v['url'];?>" title="<?php echo $v['title'];?>" <?php echo $g['urlnew'];?>></a>
                                    </figcaption>
                                </figure>
                            </div>
                        <?php endforeach;?>
                    </div>
                </div>
                <div class="bottom-box invisible" data-plugin="appear" data-animate="slide-bottom" data-repeat="false">
                    <div class="button-prev button">
                        <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                    </div>
                    <div class="button-next button">
                        <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                    </div>
                </div>
            </div>
        </div>
    </div>
</div>