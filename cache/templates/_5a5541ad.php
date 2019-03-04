<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    if($_M['form']['pageset']){
        $pullpage_id = explode("<m",$ui['pullpage_id']);
        $pullpage_id = $pullpage_id[0];
        
    }else{
        $pullpage_id = $ui['pullpage_id'];
    }
?>
<div data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>"  data-fun="$uicss_<?php echo $pullpage_id;?>" class="$uicss section met-index-product met-index-body met-index-newproduct     <?php if($ui['bg_type']){ ?>bgcolor<?php }else{ ?>bgpic<?php } ?>" m-id='<?php echo $ui['mid'];?>'>
    <div class="wrapper">
        <div class="">
            <div class="slideshow">
                <section class="strips">
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
                        <?php
                            $counts = count($result);
                            $box_w = 100/$counts;
                            $box_left = $m['_index'] * $box_w;
                            $animate = $m['_index']%2 == 0?"slide-top":"slide-bottom";
                            $delay = $m['_index']*100;
                        ?>
                      <article data-modh="<?php echo $box_w;?>" data-index="<?php echo $m['_index'];?>" class="strips__strip" style="width:<?php echo $box_w;?>%;left:<?php echo $box_left;?>%">
                        <div class="strip__content invisible animation-delay-<?php echo $delay;?>" data-plugin="appear" data-animate="<?php echo $animate;?>" data-repeat="false" style="background:url(<?php echo $m['columnimg'];?>) no-repeat;background-size: cover;background-position: center;">
                            <h2 class="strip__title" data-name="Lorem" m-id='<?php echo $ui['mid'];?>'><?php echo $m['name'];?></h2>
                                <?php if($ui['box_type']){ ?>
                                <div class="strip__inner-text" data-hex="<?php echo $ui['box_txt_bg'];?>|<?php echo $ui['box_txt_opt'];?>">
                                    <h4><?php echo $m['namemark'];?></h4>
                                    <?php $img=strstr($m['indeximg'],"upload"); ?>
                                        <?php if($img){ ?>
                                        <img src="<?php echo $m['indeximg'];?>" alt="<?php echo $m['name'];?>" style="max-width:100%;" />
                                    <?php } ?>
                                    <p>
                                      <?php echo $m['description'];?>
                                    </p>
                                    <div>
                                        <a     <?php if($ui['iflink']){ ?>href="<?php echo $m['url'];?>"<?php }else{ ?>href="<?php echo $ui['link'];?>"<?php } ?>  class="set_1_btn Vbtn-1" title="<?php echo $m['namemark'];?>" <?php echo $m['urlnew'];?>>
                                            <svg>
                                                <rect x="0" y="0" fill="none" width="100%" height="100%"></rect>
                                            </svg>
                                            <span><?php echo $ui['box_more_txt'];?></span>
                                        </a>
                                    </div>
                                </div>
                            <?php }else{ ?>
                                <div class="met-editor strip__inner-text">
                                    <?php echo $m['content'];?>
                                </div>
                            <?php } ?>
                        </div>
                        <i class="fa fa-close strip__close"></i>
                      </article>
                    <?php endforeach;?>
                </section>
            </div>
        </div>
    </div>
</div>