<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    if($_M['form']['pageset']){
        $pullpage_id = explode("<m",$ui['pullpage_id']);
        $pullpage_id = $pullpage_id[0];
    }else{
        $pullpage_id = $ui['pullpage_id'];
    }
?>
<div data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>" data-funname="about" data-fun="$uicss_<?php echo $pullpage_id;?>" class="$uicss abouts section met-index-body     <?php if($ui['bg_type']){ ?>bgcolor<?php }else{ ?>bgpic<?php } ?>" m-id='<?php echo $ui['mid'];?>'>
	<div class="container">
        <div class="row">
            <div class="about_txt">
                <div class="col-sm-6 invisible about-text" data-plugin="appear" data-animate="slide-left" data-repeat="false" m-id='<?php echo $ui['mid'];?>'>
                    <h3 class="about_title">
                        <?php echo $ui['left_title'];?>
                    </h3>
                    <p class="about_desc">
                        <?php echo $ui['left_desc'];?>
                    </p>
                    <a href="<?php echo $ui['left_more_a'];?>" class="set_1_btn Vbtn-1" title="<?php echo $ui['left_title'];?>">
                        <svg>
                            <rect x="0" y="0" fill="none" width="100%" height="100%"></rect>
                        </svg>
                        <span><?php echo $ui['left_more'];?></span>
                    </a>
                </div>
                <div class="col-sm-6 invisible about-img" data-plugin="appear" data-animate="slide-right" data-repeat="false">
                    <?php $img=strstr($ui['right_img'],"http"); ?>
                        <?php if($img){ ?>
                        <img src="<?php echo $ui['right_img'];?>" alt="<?php echo $ui['left_title'];?>" style="max-width:100%;">
                    <?php } ?>
                </div>
            </div>
            <div class="about_num " data-plugin="appear" data-animate="slide-bottom" data-repeat="false">
                    <?php if($ui['num_title1']){ ?>
                    <div class="about_num_box col-xs-6 col-sm-3">
                        <div class="about_num_pad">
                            <div class="about_num_icon col-xs-4">
                                <?php $img=strstr($ui['num_icon1'],"http"); ?>
                                    <?php if($img){ ?>
                                    <img src="<?php echo $ui['num_icon1'];?>" alt="<?php echo $ui['num_title1'];?>">
                                <?php } ?>
                            </div>
                            <div class="about_num_txt col-xs-8">
                                <h4 class="about_num_txt_title"><?php echo $ui['num_title1'];?></h4>
                                <div class="about_num_txt_num_box">
                                    <div class="about_num_txt_num" data-parameter="<?php echo $ui['num_box1'];?>" data-Method="$uicss_<?php echo $pullpage_id;?>">000</div>
                                    <!-- <span class="about_num_txt_icon">+</span> -->
                                </div>
                            </div>
                        </div>
                    </div>
                <?php } ?>
                    <?php if($ui['num_title2']){ ?>
                    <div class="about_num_box col-sm-3 col-xs-6">
                        <div class="about_num_pad">
                            <div class="about_num_icon col-xs-4">
                                <?php $img=strstr($ui['num_icon2'],"http"); ?>
                                    <?php if($img){ ?>
                                <img src="<?php echo $ui['num_icon2'];?>" alt="<?php echo $ui['num_title2'];?>">
                                <?php } ?>
                            </div>
                            <div class="about_num_txt col-xs-8">
                                <h4 class="about_num_txt_title"><?php echo $ui['num_title2'];?></h4>
                                <div class="about_num_txt_num_box">
                                    <div class="about_num_txt_num" data-parameter="<?php echo $ui['num_box2'];?>" data-Method="$uicss_<?php echo $pullpage_id;?>">000</div>
                                    <!-- <span class="about_num_txt_icon">+</span> -->
                                </div>
                            </div>
                        </div>
                    </div>
                <?php } ?>
                    <?php if($ui['num_title3']){ ?>
                    <div class="about_num_box col-sm-3 col-xs-6">
                        <div class="about_num_pad">
                            <div class="about_num_icon col-xs-4">
                                <?php $img=strstr($ui['num_icon3'],"http"); ?>
                                    <?php if($img){ ?>
                                <img src="<?php echo $ui['num_icon3'];?>" alt="<?php echo $ui['num_title3'];?>">
                                <?php } ?>
                            </div>
                            <div class="about_num_txt col-xs-8">
                                <h4 class="about_num_txt_title"><?php echo $ui['num_title3'];?></h4>
                                <div class="about_num_txt_num_box">
                                    <div class="about_num_txt_num" data-parameter="<?php echo $ui['num_box3'];?>" data-Method="$uicss_<?php echo $pullpage_id;?>">000</div>
                                    <!-- <span class="about_num_txt_icon">+</span> -->
                                </div>
                            </div>
                        </div>
                    </div>
                <?php } ?>
                    <?php if($ui['num_title4']){ ?>
                    <div class="about_num_box col-sm-3 col-xs-6">
                        <div class="about_num_pad">
                            <div class="about_num_icon col-xs-4">
                                <?php $img=strstr($ui['num_icon4'],"http"); ?>
                                    <?php if($img){ ?>
                                <img src="<?php echo $ui['num_icon4'];?>" alt="<?php echo $ui['num_title4'];?>">
                                <?php } ?>
                            </div>
                            <div class="about_num_txt col-xs-8">
                                <h4 class="about_num_txt_title"><?php echo $ui['num_title4'];?></h4>
                                <div class="about_num_txt_num_box">
                                    <div class="about_num_txt_num" data-parameter="<?php echo $ui['num_box4'];?>" data-Method="$uicss_<?php echo $pullpage_id;?>">000</div>
                                    <!-- <span class="about_num_txt_icon">+</span> -->
                                </div>
                            </div>
                        </div>
                    </div>
                <?php } ?>
            </div>
        </div>
    </div>
</div>