
        <?php
            $id = 7;
            $style = "met_21_6";
            if(!isset($ui_compile)){
                load::sys_class('view/ui_compile');
                $ui_compile = new ui_compile();
            }
            $ui = $ui_compile->list_local_config($id);
            $ui['has'] =$ui_compile->list_page_config($met_page);
            ?>
<?php
    if($_M['form']['pageset']){
        $pullpage_id = explode("<m",$ui['pullpage_id']);
        $pullpage_id = $pullpage_id[0];
    }else{
        $pullpage_id = $ui['pullpage_id'];
    }
    $index_ok = $data['classnow']== 10001?"index_ok":"ny_ok";
?>
<div data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>" data-fun="foot_info_met_21_6_<?php echo $pullpage_id;?>" data-funname="foot" class=" <?php echo $ui['ny_foot_bg_type'];?> <?php echo $index_ok;?> foot_info_met_21_6 abouts section met-index-body     <?php if($ui['bg_type']){ ?>bgcolor<?php }else{ ?>bgpic<?php } ?>" m-id='<?php echo $ui['mid'];?>'>
	<div class="">
        <div class="">
            <div class="wrapper-box loading" data-Method="foot_info_met_21_6_<?php echo $pullpage_id;?>">
                <div id="wrapper">
                    <div id="bg"></div>
                    <div id="overlay" class="    <?php if($ui[mask_ok]){ ?>mask_ok<?php } ?>"></div>
                    <div id="main">
                        <!-- Header -->
                            <div id="header" m-id='<?php echo $ui['mid'];?>'>
                                <h2><?php echo $ui['foot_title'];?></h2>
                                <p><?php echo $ui['foot_desc'];?></p>
                                <nav>
                                    <ul>
                                            <?php if($ui['footinfo_qq_ok']){ ?>
                                            <li>
                                                <a class="icon style2 fa-qq"
                                                        <?php if($ui['footinfo_qq_type']==1){ ?>
                                                    href="http://crm2.qq.com/page/portalpage/wpa.php?uin=<?php echo $ui['footinfo_qq'];?>&aty=0&a=0&curl=&ty=1"
                                                    <?php }else{ ?>
                                                    href="http://wpa.qq.com/msgrd?v=3&uin=<?php echo $ui['footinfo_qq'];?>&site=qq&menu=yes"
                                                    <?php } ?>
                                                    rel="nofollow" target="_blank">
                                                </a>
                                            </li>
                                        <?php } ?>
                                            <?php if($ui['footinfo_wx_ok']){ ?>
                                            <li>
                                                <?php $img=strstr($ui['footinfo_wx'],"http"); ?>
                                                <a  class="icon style2 fa-weixin" id="met-weixin" data-plugin="webuiPopover" data-trigger="hover" data-animation="pop" data-placement='top' data-width='155' data-padding='0' data-content="<div class='text-xs-center'>    <?php if($img){ ?><img src='<?php echo $ui['footinfo_wx'];?>' alt='<?php echo $c['met_webname'];?>' width='150' height='150' id='met-weixin-img'><?php } ?></div>"></a>
                                            </li>
                                        <?php } ?>
                                            <?php if($ui['weibo_ok']){ ?>
                                            <li class="box-social">
                                                <a class="icon style2 fa-weibo" href="<?php echo $ui['weibo_url'];?>" rel="nofollow" target="_blank">
                                                </a>
                                            </li>
                                        <?php } ?>
                                            <?php if($ui['facebook_ok']){ ?>
                                            <li class="box-social">
                                                <a class="icon style2 fa-facebook" href="<?php echo $ui['facebook_url'];?>" rel="nofollow" target="_blank">
                                                </a>
                                            </li>
                                        <?php } ?>
                                            <?php if($ui['emil_ok']){ ?>
                                            <li class="box-social">
                                                <a class="icon style2 fa-envelope-o" href="mailto:<?php echo $ui['emil_url'];?>" rel="nofollow" target="_blank">
                                                </a>
                                            </li>
                                        <?php } ?>
                                    </ul>
                                </nav>
                                <ul class="met-langlist p-0">
                                        <?php if($c['met_lang_mark'] && $ui[langlist_ok]){ ?>
                                        <li class="vertical-align m-x-5" m-id='lang' m-type='lang'>
                                            <div class="inline-block dropup">
                                                <?php
    $language = load::sys_class('label', 'new')->get('language')->get_lang();
    $sub = count($language);
    $i = 0;
    foreach($language as $index=>$v):

        $v['_index']   = $index;
        $v['_first']   = $i == 0 ? true:false;

        $v['_last']    = $index == (count($language)-1) ? true : false;
        $v['sub'] = $sub;
        $i++;
?>
                                                        <?php if($data['lang']==$v['mark']){ ?>
                                                        <button type="button" data-toggle="dropdown" class="btn btn-outline btn-default btn-squared dropdown-toggle btn-lang">
                                                                <?php if($ui['langlist_icon_ok']){ ?>
                                                                <img src="<?php echo $v['flag'];?>" alt="<?php echo $v['name'];?>" style="max-width:100%;">
                                                            <?php } ?>
                                                            <span ><?php echo $v['name'];?></span>
                                                        </button>
                                                    <?php } ?>
                                                <?php endforeach;?>
                                                <ul class="dropdown-menu dropdown-menu-right animate animate-reverse" id="met-langlist-dropdown" role="menu">
                                                    <?php
    $language = load::sys_class('label', 'new')->get('language')->get_lang();
    $sub = count($language);
    $i = 0;
    foreach($language as $index=>$v):

        $v['_index']   = $index;
        $v['_first']   = $i == 0 ? true:false;

        $v['_last']    = $index == (count($language)-1) ? true : false;
        $v['sub'] = $sub;
        $i++;
?>
                                                        <a href="<?php echo $v['met_weburl'];?>" title="<?php echo $v['name'];?>" class='dropdown-item'>
                                                                <?php if($ui['langlist_icon_ok']){ ?>
                                                            <img src="<?php echo $v['flag'];?>" alt="<?php echo $v['name'];?>" style="max-width:100%;">
                                                            <?php } ?>
                                                            <?php echo $v['name'];?>
                                                        </a>
                                                    <?php endforeach;?>
                                                </ul>
                                            </div>
                                        </li>
                                    <?php } ?>
                                        <?php if($c['met_ch_lang'] && $ui['simplified']){ ?>
                                            <?php if($data[lang]==cn){ ?>
                                            <li class="met-s2t  vertical-align nav-item m-x-5" m-id="lang" m-type="lang">
                                            <div class="inline-block">
                                                <button type="button" class="btn btn-outline btn-default btn-squared btn-lang btn-cntotc" data-tolang='tc'>繁体</button>
                                                <?php }else if($data[lang]==tc){ ?>
                                                <button type="button" class="btn btn-outline btn-default btn-squared btn-lang btn-cntotc" data-tolang='cn'>简体</button>
                                            </div>
                                        </li>
                                        <?php } ?>
                                    <?php } ?>
                                </ul>
                            </div>

                        <!-- Footer -->
                            <footer id="footer">
                                    <?php if($ui['link_ok']){ ?>
                                    <ul class="breadcrumb p-0 link-img m-0">
                                        <li class='breadcrumb-item'><?php echo $ui['link_txt'];?> :</li>
                                        <?php
    $result = load::sys_class('label', 'new')->get('link')->get_link_list();
    $sub = count($result);
     foreach($result as $index=>$v):
        $v['sub']      = $sub;
        $v['_index']   = $index;
        $v['_first']   = $index == 0 ? true:false;
        $v['_last']    = $index == (count($result)-1) ? true : false;
        $v['nofollow'] = $v['nofollow'] ? "rel='nofollow'" : '';
?>
                                            <li class='breadcrumb-item     <?php if($ui[split]){ ?>split<?php } ?>'>
                                                <a href="<?php echo $v['weburl'];?>" title="<?php echo $v['webname'];?>" <?php echo $v['nofollow'];?> target="_blank">
                                                        <?php if($v['link_type']==1){ ?>
                                                        <img src="<?php echo $v['weblogo'];?>" alt="<?php echo $v['webname'];?>" height='40'>
                                                    <?php }else{ ?>
                                                        <span><?php echo $v['webname'];?></span>
                                                    <?php } ?>
                                                </a>
                                            </li>
                                        <?php endforeach;?>
                                    </ul>
                                <?php } ?>
                                <div class="foot_icon">
                                        <?php if($c['met_footright']){ ?>
                                        <span><?php echo $c['met_footright'];?></span>
                                    <?php } ?>
                                        <?php if($c['met_footaddress']){ ?>
                                        <span><?php echo $c['met_footaddress'];?></span>
                                    <?php } ?>
                                        <?php if($c['met_foottel']){ ?>
                                        <span><?php echo $c['met_foottel'];?></span>
                                    <?php } ?>
                                        <?php if($c['met_foottext']){ ?>
                                        <span class="hide"><?php echo $c['met_foottext'];?></span>
                                    <?php } ?>
                                        <?php if($c['met_footother']){ ?>
                                        <div>
                                            <?php echo $c['met_footother'];?>
                                        </div>
                                    <?php } ?>
                                </div>
                                <div class="powered_by_metinfo">
                                    <?php echo $c['met_agents_copyright_foot'];?>
                                </div>
                            </footer>
                    </div>
                </div>
            </div>
        </div>
    </div>
</div>

<?php if($lang_json_file_ok){ ?>
<input type="hidden" name="met_lazyloadbg" value="<?php echo $g['lazyloadbg'];?>">
<?php if($c["shopv2_open"]){ ?>
<script>
var jsonurl="<?php echo $url['shop_cart_jsonlist'];?>",
    totalurl="<?php echo $url['shop_cart_modify'];?>",
    delurl="<?php echo $url['shop_cart_del'];?>",
    price_prefix="<?php echo $c['shopv2_price_str_prefix'];?>",
    price_suffix="<?php echo $c['shopv2_price_str_suffix'];?>";
</script>
<?php
    }
}
$basic_js_name=$metinfover_v2?"":"_web";
?>
<script src="<?php echo $c['met_weburl'];?>public/ui/v2/static/js/basic<?php echo $basic_js_name;?>.js?<?php echo $met_file_version;?>"></script>
<?php
if($lang_json_file_ok){
    if($metinfover_v2){
        if(file_exists(PATH_TEM."cache/common.js")){
            $common_js_time = filemtime(PATH_TEM."cache/common.js");
            $metpagejs="common.js?".$common_js_time;
        }
        if($met_page){
            $page_js_time = filemtime(PATH_TEM."cache/".$met_page."_".$_M["lang"].".js");
            $metpagejs=$met_page."_".$_M["lang"].".js?".$page_js_time;
        }
?>
<script>
var metpagejs="<?php echo $c['met_weburl'];?>templates/<?php echo $c['met_skin_user'];?>/cache/<?php echo $metpagejs;?>";
if(typeof jQuery != "undefined"){
    metPageJs(metpagejs);
}else{
    var metPageInterval=setInterval(function(){
        if(typeof jQuery != "undefined"){
            metPageJs(metpagejs);
            clearInterval(metPageInterval);
        }
    },50)
}
</script>
<?php
    }
    $met_lang_time = filemtime(PATH_WEB."cache/lang_json_".$data["lang"].".js");
?>
<script src="<?php echo $c['met_weburl'];?>cache/lang_json_<?php echo $data['lang'];?>.js?<?php echo $met_lang_time;?>"></script>
<?php
    if($c["shopv2_open"]){
?>
<script src="<?php echo $c['met_weburl'];?>app/app/shop/web/templates/met/js/own.js?<?php echo $met_file_version;?>"></script>
<?php
    }
    if(is_mobile() && $c["met_footstat_mobile"]){
?>
<?php echo $c['met_footstat_mobile'];?>

<?php }else if(!is_mobile() && $c["met_footstat"]){?>
<?php echo $c['met_footstat'];?>

<?php
    }
    if($_M["html_plugin"]["foot_script"]){
?>
<?php echo $_M["html_plugin"]["foot_script"];?>

<?php
    }
}
?>
</body>
</html>