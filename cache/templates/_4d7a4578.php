<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    $type=strtolower(trim('current'));
    $cid=$data['classnow'];
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
if($_M['form']['pageset']){
    $pullpage_id = explode("<m",$ui['pullpage_id']);
    $pullpage_id = $pullpage_id[0];
}else{
    $pullpage_id = $ui['pullpage_id'];
}
$ui['listhide']=explode('|', $ui['listhide']);
$ui['detailhide']=explode('|', $ui['detailhide']);
if($data['name']){
    foreach ($ui['listhide'] as $val) {
        if($val==$data['name']){
            $hide=0;
            break;
        }else{
            $hide=1;
        }
    }
}
if($data['title']){
    foreach ($ui['detailhide'] as $val) {
        if($val==$m['name']){
            $hide=0;
            break;
        }else{
            $hide=1;
        }
    }
}
?>
<?php endforeach;?>
    <?php if($hide){ ?>
<?php 
    $banner = load::sys_class('label', 'new')->get('banner')->get_column_banner($data['classnow']);
    $sub = count($banner['img']);
    foreach($banner['img'] as $index=>$v):
        $v['_index']   = $index;
        $v['_first']   = $index == 0 ? true:false;
        $v['_last']    = $index == (count($result)-1) ? true : false;
        $v['type'] = $banner['config']['type'];
        $v['y'] = $banner['config']['y'];
        $v['sub'] = $sub;
?><?php endforeach;?>
    <?php if($sub || $data['classnow']==10001){ ?>
<div  data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>" data-fun="$uicss_<?php echo $pullpage_id;?>" data-funname="banner" class="section $uicss     <?php if($data['classnow']<>10001){ ?>banner-ny-h<?php } ?>"
    m-id='<?php echo $ui['mid'];?>' m-type="banner" data-hash="<?php echo $ui['hash'];?>" data-title="<?php echo $ui['hash'];?>">
        <?php if($ui['video'] && $data['classnow']==10001){ ?>
        <div class="banner_met_ met-video" data-Method="$uicss_<?php echo $pullpage_id;?>">
            <div class="hide"><?php echo $ui['video'];?></div>
            <video src="" type="video/mp4"  loop id="media" autoplay muted></video>
            <div class="video-bg" style="background-image:url(<?php echo $ui['bg_video'];?>);background-size:cover;background-position:center;background-repeat:no-repeat;"></div>
        </div>
    <?php } ?>
    <div class="banner">
        <div class="banner-warpper">
            <?php 
    $banner = load::sys_class('label', 'new')->get('banner')->get_column_banner($data['classnow']);
    $sub = count($banner['img']);
    foreach($banner['img'] as $index=>$v):
        $v['_index']   = $index;
        $v['_first']   = $index == 0 ? true:false;
        $v['_last']    = $index == (count($result)-1) ? true : false;
        $v['type'] = $banner['config']['type'];
        $v['y'] = $banner['config']['y'];
        $v['sub'] = $sub;
?>
            <?php if($data['classnow']==10001){ ?>
            
            <div class="banner-item"  
                style="background-image:url(<?php echo $v['img_path'];?>);background-size:cover;height: 100%;"
                data-height="<?php echo $v['height'];?>|<?php echo $v['height_t'];?>|<?php echo $v['height_m'];?>"
                data-autoplayspeed="<?php echo $ui['autoplaySpeed'];?>" 
                data-src="<?php echo $v['img_path'];?>"
                data-speed="<?php echo $ui['speed'];?>">
        <?php }else{ ?>
            <div 
                style="height: 100%;" 
                class="banner-item"
                data-height="<?php echo $v['height'];?>|<?php echo $v['height_t'];?>|<?php echo $v['height_m'];?>"
                data-autoplayspeed="<?php echo $ui['autoplaySpeed'];?>" 
                data-src="<?php echo $v['img_path'];?>"
                data-speed="<?php echo $ui['speed'];?>">
                <img src="<?php echo $v['img_path'];?>" alt="">
        <?php } ?>
                    <?php if($v['img_title'] || $v['img_link']){ ?>
                <div class="banner-text p-<?php echo $v['img_text_position'];?>" >
                    <div class='container' m-id='<?php echo $ui['mid'];?>' m-type="banner">
                        <div class='banner-text-con'>
                            <div>
                                <h3 class="title ani font-weight-500" 
                                swiper-animate-effect="fadeInUp" 
                                swiper-animate-duration="1s" 
                                swiper-animate-delay="0s"
                                style="color:<?php echo $v['img_title_color'];?>">
                                    <?php echo $v['img_title'];?>
                                </h3>
                                <p class="subtitle ani" 
                                swiper-animate-effect="fadeInUp" 
                                swiper-animate-duration="1s" 
                                swiper-animate-delay="0.5s"
                                style='color:<?php echo $v['img_des_color'];?>'>
                                <?php echo $v['img_des'];?>
                                </p>
                                <span class="line ani" 
                                swiper-animate-effect="fadeInUp" 
                                swiper-animate-duration="1s" 
                                swiper-animate-delay="0.7s">
                                </span>
                                <p class="banner-icon ani"                                
                                swiper-animate-effect="fadeIn" 
                                swiper-animate-duration="1s" 
                                swiper-animate-delay="0.9s">
                                        <?php if($ui['tel']){ ?>                
                                        <i class="icon pe-call"></i>&nbsp;<?php echo $ui['tel'];?>
                                    <?php } ?>
                                        <?php if($ui['tel'] && $ui['mail']){ ?>&nbsp;|&nbsp;<?php } ?>
                                        <?php if($ui['mail']){ ?>
                                        <i class="icon pe-mail"></i>&nbsp;<?php echo $ui['mail'];?>
                                    <?php } ?>
                                </p>
                                    <?php if($v['img_link']){ ?>
                                    <a href="<?php echo $v['img_link'];?>" title="<?php echo $v['img_des'];?>" class="more ani" swiper-animate-effect="fadeIn" swiper-animate-duration="1s" swiper-animate-delay="1s" target="_blank" >
                                        <span data-title="<?php echo $ui['more'];?>"><?php echo $ui['more'];?></span>
                                    </a>
                                <?php } ?>
                            </div>
                        </div>
                    </div>
                </div>
                <?php } ?>
            </div>
            <?php endforeach;?>
        </div>
    </div>
    <div class="banner-ctrl">
        <span class="left"><i class="icon pe-angle-left"></i></span>
        <span class="right"><i class="icon pe-angle-right"></i></span>
    </div>
    <div class="banner-bg"></div>
        <?php if($ui['next']){ ?>
    <div class="banner-next" m-id='<?php echo $ui['mid'];?>' m-type="banner">
        <span class="next-text">
            <?php echo $ui['next'];?>
        </span>
        <span class="next-icon">
            <i class="icon pe-angle-down"></i>
        </span>

    </div>
<?php } ?>
</div>
<?php }else{ ?>
    <?php
    $type=strtolower(trim('current'));
    $cid=$data['class1'];
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
        <div class="$uicss-ny vertical-align text-xs-center" m-id='<?php echo $ui['mid'];?>' m-type='banner'>
            <h1 class="vertical-align-middle"><?php echo $m[name];?></h1>
        </div>
    <?php endforeach;?>
<?php } ?>
<?php } ?>