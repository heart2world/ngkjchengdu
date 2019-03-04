<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    if($_M['form']['pageset']){
        $pullpage_id = explode("<m",$ui['pullpage_id']);
        $pullpage_id = $pullpage_id[0];
    }else{
        $pullpage_id = $ui['pullpage_id'];
    }
?>
<div data-name="{$ui.pullpage_name}" data-id="{$pullpage_id}" data-funname="about" data-fun="$uicss_{$pullpage_id}" class="$uicss abouts section met-index-body <if value="$ui['bg_type']">bgcolor<else/>bgpic</if>" m-id='{$ui.mid}'>
	<div class="container">
        <div class="row">
            <div class="about_txt">
                <div class="col-sm-6 invisible about-text" data-plugin="appear" data-animate="slide-left" data-repeat="false" m-id='{$ui.mid}'>
                    <h3 class="about_title">
                        {$ui.left_title}
                    </h3>
                    <p class="about_desc">
                        {$ui.left_desc}
                    </p>
                    <a href="{$ui.left_more_a}" class="set_1_btn Vbtn-1" title="{$ui.left_title}">
                        <svg>
                            <rect x="0" y="0" fill="none" width="100%" height="100%"></rect>
                        </svg>
                        <span>{$ui.left_more}</span>
                    </a>
                </div>
                <div class="col-sm-6 invisible about-img" data-plugin="appear" data-animate="slide-right" data-repeat="false">
                    <?php $img=strstr($ui['right_img'],"http"); ?>
                    <if value="$img">
                        <img src="{$ui.right_img}" alt="{$ui.left_title}" style="max-width:100%;">
                    </if>
                </div>
            </div>
            <div class="about_num " data-plugin="appear" data-animate="slide-bottom" data-repeat="false">
                <if value="$ui['num_title1']">
                    <div class="about_num_box col-xs-6 col-sm-3">
                        <div class="about_num_pad">
                            <div class="about_num_icon col-xs-4">
                                <?php $img=strstr($ui['num_icon1'],"http"); ?>
                                <if value="$img">
                                    <img src="{$ui.num_icon1}" alt="{$ui.num_title1}">
                                </if>
                            </div>
                            <div class="about_num_txt col-xs-8">
                                <h4 class="about_num_txt_title">{$ui.num_title1}</h4>
                                <div class="about_num_txt_num_box">
                                    <div class="about_num_txt_num" data-parameter="{$ui.num_box1}" data-Method="$uicss_{$pullpage_id}">000</div>
                                    <!-- <span class="about_num_txt_icon">+</span> -->
                                </div>
                            </div>
                        </div>
                    </div>
                </if>
                <if value="$ui['num_title2']">
                    <div class="about_num_box col-sm-3 col-xs-6">
                        <div class="about_num_pad">
                            <div class="about_num_icon col-xs-4">
                                <?php $img=strstr($ui['num_icon2'],"http"); ?>
                                <if value="$img">
                                <img src="{$ui.num_icon2}" alt="{$ui.num_title2}">
                                </if>
                            </div>
                            <div class="about_num_txt col-xs-8">
                                <h4 class="about_num_txt_title">{$ui.num_title2}</h4>
                                <div class="about_num_txt_num_box">
                                    <div class="about_num_txt_num" data-parameter="{$ui.num_box2}" data-Method="$uicss_{$pullpage_id}">000</div>
                                    <!-- <span class="about_num_txt_icon">+</span> -->
                                </div>
                            </div>
                        </div>
                    </div>
                </if>
                <if value="$ui['num_title3']">
                    <div class="about_num_box col-sm-3 col-xs-6">
                        <div class="about_num_pad">
                            <div class="about_num_icon col-xs-4">
                                <?php $img=strstr($ui['num_icon3'],"http"); ?>
                                <if value="$img">
                                <img src="{$ui.num_icon3}" alt="{$ui.num_title3}">
                                </if>
                            </div>
                            <div class="about_num_txt col-xs-8">
                                <h4 class="about_num_txt_title">{$ui.num_title3}</h4>
                                <div class="about_num_txt_num_box">
                                    <div class="about_num_txt_num" data-parameter="{$ui.num_box3}" data-Method="$uicss_{$pullpage_id}">000</div>
                                    <!-- <span class="about_num_txt_icon">+</span> -->
                                </div>
                            </div>
                        </div>
                    </div>
                </if>
                <if value="$ui['num_title4']">
                    <div class="about_num_box col-sm-3 col-xs-6">
                        <div class="about_num_pad">
                            <div class="about_num_icon col-xs-4">
                                <?php $img=strstr($ui['num_icon4'],"http"); ?>
                                <if value="$img">
                                <img src="{$ui.num_icon4}" alt="{$ui.num_title4}">
                                </if>
                            </div>
                            <div class="about_num_txt col-xs-8">
                                <h4 class="about_num_txt_title">{$ui.num_title4}</h4>
                                <div class="about_num_txt_num_box">
                                    <div class="about_num_txt_num" data-parameter="{$ui.num_box4}" data-Method="$uicss_{$pullpage_id}">000</div>
                                    <!-- <span class="about_num_txt_icon">+</span> -->
                                </div>
                            </div>
                        </div>
                    </div>
                </if>
            </div>
        </div>
    </div>
</div>