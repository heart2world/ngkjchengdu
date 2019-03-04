<?php defined('IN_MET') or exit('No permission'); ?>
<tag action='category' cid="$data['classnow']" type='current'>
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
</tag>
<if value="$hide">
<tag action="banner.list"></tag>
<if value="$sub || $data['classnow'] eq 10001">
<div  data-name="{$ui.pullpage_name}" data-id="{$pullpage_id}" data-fun="$uicss_{$pullpage_id}" data-funname="banner" class="section $uicss <if value="$data['classnow'] neq 10001">banner-ny-h</if>"
    m-id='{$ui.mid}' m-type="banner" data-hash="{$ui.hash}" data-title="{$ui.hash}">
    <if value="$ui['video'] && $data['classnow'] eq 10001">
        <div class="banner_met_ met-video" data-Method="$uicss_{$pullpage_id}">
            <div class="hide">{$ui.video}</div>
            <video src="" type="video/mp4"  loop id="media" autoplay muted></video>
            <div class="video-bg" style="background-image:url({$ui.bg_video});background-size:cover;background-position:center;background-repeat:no-repeat;"></div>
        </div>
    </if>
    <div class="banner">
        <div class="banner-warpper">
            <tag action="banner.list">
        <if value="$data['classnow'] eq 10001">
            
            <div class="banner-item"  
                style="background-image:url({$v.img_path});background-size:cover;height: 100%;"
                data-height="{$v.height}|{$v.height_t}|{$v.height_m}"
                data-autoplayspeed="{$ui.autoplaySpeed}" 
                data-src="{$v.img_path}"
                data-speed="{$ui.speed}">
        <else/>
            <div 
                style="height: 100%;" 
                class="banner-item"
                data-height="{$v.height}|{$v.height_t}|{$v.height_m}"
                data-autoplayspeed="{$ui.autoplaySpeed}" 
                data-src="{$v.img_path}"
                data-speed="{$ui.speed}">
                <img src="{$v.img_path}" alt="">
        </if>
                <if value="$v['img_title'] || $v['img_link']">
                <div class="banner-text p-{$v.img_text_position}" >
                    <div class='container' m-id='{$ui.mid}' m-type="banner">
                        <div class='banner-text-con'>
                            <div>
                                <h3 class="title ani font-weight-500" 
                                swiper-animate-effect="fadeInUp" 
                                swiper-animate-duration="1s" 
                                swiper-animate-delay="0s"
                                style="color:{$v.img_title_color}">
                                    {$v.img_title}
                                </h3>
                                <p class="subtitle ani" 
                                swiper-animate-effect="fadeInUp" 
                                swiper-animate-duration="1s" 
                                swiper-animate-delay="0.5s"
                                style='color:{$v.img_des_color}'>
                                {$v.img_des}
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
                                    <if value="$ui['tel']">                
                                        <i class="icon pe-call"></i>&nbsp;{$ui.tel}
                                    </if>
                                    <if value="$ui['tel'] && $ui['mail']">&nbsp;|&nbsp;</if>
                                    <if value="$ui['mail']">
                                        <i class="icon pe-mail"></i>&nbsp;{$ui.mail}
                                    </if>
                                </p>
                                <if value="$v['img_link']">
                                    <a href="{$v.img_link}" title="{$v.img_des}" class="more ani" swiper-animate-effect="fadeIn" swiper-animate-duration="1s" swiper-animate-delay="1s" target="_blank" >
                                        <span data-title="{$ui.more}">{$ui.more}</span>
                                    </a>
                                </if>
                            </div>
                        </div>
                    </div>
                </div>
                </if>
            </div>
            </tag>
        </div>
    </div>
    <div class="banner-ctrl">
        <span class="left"><i class="icon pe-angle-left"></i></span>
        <span class="right"><i class="icon pe-angle-right"></i></span>
    </div>
    <div class="banner-bg"></div>
    <if value="$ui['next']">
    <div class="banner-next" m-id='{$ui.mid}' m-type="banner">
        <span class="next-text">
            {$ui.next}
        </span>
        <span class="next-icon">
            <i class="icon pe-angle-down"></i>
        </span>

    </div>
</if>
</div>
<else/>
    <tag action='category' type="current" cid="$data['class1']">
        <div class="$uicss-ny vertical-align text-xs-center" m-id='{$ui.mid}' m-type='banner'>
            <h1 class="vertical-align-middle">{$m[name]}</h1>
        </div>
    </tag>
</if>
</if>