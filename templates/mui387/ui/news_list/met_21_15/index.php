<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    if($_M['form']['pageset']){
        $pullpage_id = explode("<m",$ui['pullpage_id']);
        $pullpage_id = $pullpage_id[0];
    }else{
        $pullpage_id = $ui['pullpage_id'];
    }
?>
<div data-name="{$ui.pullpage_name}" data-id="{$pullpage_id}" data-fun="$uicss_{$pullpage_id}" class="$uicss abouts section met-index-body <if value="$ui['bg_type']">bgcolor<else/>bgpic</if>" m-id='{$ui.mid}'>
    <div class="">
        <div class="">
            <div class="hero-slides" m-id='{$ui.mid}'>
                <div class="title_box">
                    <if value="$ui['title']">
                        <h3 class="invisible" data-plugin="appear" data-animate="slide-top" data-repeat="false">
                            <tag action="category" cid="$ui['id']" type="current">
                                <a href="{$m.url}" title="{$ui.title}" {$m.urlnew}>{$ui.title}</a>
                            </tag>
                        </h3>
                    </if>
                    <if value="$ui['desc']">
                        <p class="invisible" data-plugin="appear" data-animate="slide-bottom" data-repeat="false">{$ui.desc}</p>
                    </if>
                </div>
                <div class="swiper-container invisible" data-num="{$ui.list_nums}" data-listnum="{$ui.list_num}" data-plugin="appear" data-animate="slide-bottom" data-repeat="false">
                    <div class="swiper-wrapper grid">
                        <tag action="list" cid="$ui['id']" type="$ui['list_type']" num="$ui['list_num']">
                            <div class="swiper-slide">
                                <figure class="effect-sarah">
                                    <img src="{$v.imgurl|thumb:$ui['img_w'],$ui['img_h']}" alt="{$v.title}"/>
                                    <figcaption>
                                        <h4>{$v.title}</h4>
                                        <p>{$v.description}</p>
                                        <a href="{$v.url}" title="{$v.title}" {$g.urlnew}></a>
                                    </figcaption>
                                </figure>
                            </div>
                        </tag>
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