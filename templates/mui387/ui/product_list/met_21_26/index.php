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
    <div class="product_box">
        <div class="row">
            <div class="hero-slides">
                <div class="headers" >
                    <tag action="category" cid="$ui['id']" type="current">
                        <div class="logos invisible" data-plugin="appear" data-animate="slide-left" data-repeat="false">
                            <if value="$ui['title']">
                                <h3>
                                    <a href="{$m.url}" title="{$ui.title}" {$m.urlnew}>
                                        {$ui.title}
                                    </a>
                                </h3>
                            </if>
                            <if value="$ui['desc']">
                                <p>{$ui.desc}</p>
                            </if>
                        </div>
                        <a href="{$m.url}" title="{$ui.title}" class="invisible" data-plugin="appear" data-animate="slide-right" data-repeat="false" {$m.urlnew} >
                            <div class="menu">
                                <span class="more">{$ui.box_more}</span>
                                <div class="hamburgers">
                                    <div class="slice"></div>
                                    <div class="slice"></div>
                                    <div class="slice"></div>
                                </div>
                            </div>
                        </a>
                    </tag>
                </div>
                <div class="swiper-container invisible" m-id='{$ui.mid}' data-num="{$ui.list_nums}" data-listnum="{$ui.list_num}" data-plugin="appear" data-animate="slide-right" data-repeat="false">
                    <div class="swiper-wrapper">
                            <tag action="list" cid="$ui['id']" type="$ui['list_type']" num="$ui['list_num']">
                                <?php   ?>
                                <div class="swiper-slide" <if value="$ui['img_type']">style="background-image: url({$v.imgurl});"</if>>
                                    <if value="!$ui['img_type']">
                                        <img src="{$v.imgurl|thumb:$ui['img_w'],$ui['img_h']}" alt="{$v.title}" style="max-width:100%;">
                                    </if>
                                    <?php  $indexe = $v['_index'] >= 9?$v['_index']+1:"0".($v['_index']+1); ?>
                                    <div class="number">{$indexe}</div>
                                    <div class="body">
                                        <div class="location">
                                            <p>{$v.description}</p>
                                            <if value="$v['price_str']">
                                                <p class="price">{$v.price_str}</p>
                                            </if>
                                        </div>
                                        <div class="headline">{$v.title}</div>
                                        <a href="{$v.url}" title="{$v.title}" {$g.urlnew}>
                                            <span class="link">{$ui.more}</span>
                                        </a>
                                    </div>
                                </div>
                            </tag>
                    </div>
                    <div class="button-next button" >
                        <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                    </div>
                    <div class="button-prev button">
                        <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                    </div>
                </div>
            </div>
        </div>
    </div>
</div>