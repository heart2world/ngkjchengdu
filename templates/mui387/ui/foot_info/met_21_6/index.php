<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    if($_M['form']['pageset']){
        $pullpage_id = explode("<m",$ui['pullpage_id']);
        $pullpage_id = $pullpage_id[0];
    }else{
        $pullpage_id = $ui['pullpage_id'];
    }
    $index_ok = $data['classnow']== 10001?"index_ok":"ny_ok";
?>
<div data-name="{$ui.pullpage_name}" data-id="{$pullpage_id}" data-fun="$uicss_{$pullpage_id}" data-funname="foot" class=" {$ui.ny_foot_bg_type} {$index_ok} $uicss abouts section met-index-body <if value="$ui['bg_type']">bgcolor<else/>bgpic</if>" m-id='{$ui.mid}'>
	<div class="">
        <div class="">
            <div class="wrapper-box loading" data-Method="$uicss_{$pullpage_id}">
                <div id="wrapper">
                    <div id="bg"></div>
                    <div id="overlay" class="<if value='$ui[mask_ok]'>mask_ok</if>"></div>
                    <div id="main">
                        <!-- Header -->
                            <div id="header" m-id='{$ui.mid}'>
                                <h2>{$ui.foot_title}</h2>
                                <p>{$ui.foot_desc}</p>
                                <nav>
                                    <ul>
                                        <if value="$ui['footinfo_qq_ok']">
                                            <li>
                                                <a class="icon style2 fa-qq"
                                                    <if value="$ui['footinfo_qq_type'] eq 1">
                                                    href="http://crm2.qq.com/page/portalpage/wpa.php?uin={$ui.footinfo_qq}&aty=0&a=0&curl=&ty=1"
                                                    <else/>
                                                    href="http://wpa.qq.com/msgrd?v=3&uin={$ui.footinfo_qq}&site=qq&menu=yes"
                                                    </if>
                                                    rel="nofollow" target="_blank">
                                                </a>
                                            </li>
                                        </if>
                                        <if value="$ui['footinfo_wx_ok']">
                                            <li>
                                                <?php $img=strstr($ui['footinfo_wx'],"http"); ?>
                                                <a  class="icon style2 fa-weixin" id="met-weixin" data-plugin="webuiPopover" data-trigger="hover" data-animation="pop" data-placement='top' data-width='155' data-padding='0' data-content="<div class='text-xs-center'><if value='$img'><img src='{$ui.footinfo_wx}' alt='{$c.met_webname}' width='150' height='150' id='met-weixin-img'></if></div>"></a>
                                            </li>
                                        </if>
                                        <if value="$ui['weibo_ok']">
                                            <li class="box-social">
                                                <a class="icon style2 fa-weibo" href="{$ui.weibo_url}" rel="nofollow" target="_blank">
                                                </a>
                                            </li>
                                        </if>
                                        <if value="$ui['facebook_ok']">
                                            <li class="box-social">
                                                <a class="icon style2 fa-facebook" href="{$ui.facebook_url}" rel="nofollow" target="_blank">
                                                </a>
                                            </li>
                                        </if>
                                        <if value="$ui['emil_ok']">
                                            <li class="box-social">
                                                <a class="icon style2 fa-envelope-o" href="mailto:{$ui.emil_url}" rel="nofollow" target="_blank">
                                                </a>
                                            </li>
                                        </if>
                                    </ul>
                                </nav>
                                <ul class="met-langlist p-0">
                                    <if value="$c['met_lang_mark'] && $ui[langlist_ok]">
                                        <li class="vertical-align m-x-5" m-id='lang' m-type='lang'>
                                            <div class="inline-block dropup">
                                                <lang>
                                                    <if value="$data['lang'] eq $v['mark']">
                                                        <button type="button" data-toggle="dropdown" class="btn btn-outline btn-default btn-squared dropdown-toggle btn-lang">
                                                            <if value="$ui['langlist_icon_ok']">
                                                                <img src="{$v.flag}" alt="{$v.name}" style="max-width:100%;">
                                                            </if>
                                                            <span >{$v.name}</span>
                                                        </button>
                                                    </if>
                                                </lang>
                                                <ul class="dropdown-menu dropdown-menu-right animate animate-reverse" id="met-langlist-dropdown" role="menu">
                                                    <lang>
                                                        <a href="{$v.met_weburl}" title="{$v.name}" class='dropdown-item'>
                                                            <if value="$ui['langlist_icon_ok']">
                                                            <img src="{$v.flag}" alt="{$v.name}" style="max-width:100%;">
                                                            </if>
                                                            {$v.name}
                                                        </a>
                                                    </lang>
                                                </ul>
                                            </div>
                                        </li>
                                    </if>
                                    <if value="$c['met_ch_lang'] && $ui['simplified']">
                                        <if value="$data[lang] eq cn">
                                            <li class="met-s2t  vertical-align nav-item m-x-5" m-id="lang" m-type="lang">
                                            <div class="inline-block">
                                                <button type="button" class="btn btn-outline btn-default btn-squared btn-lang btn-cntotc" data-tolang='tc'>繁体</button>
                                                <elseif value="$data[lang] eq tc"/>
                                                <button type="button" class="btn btn-outline btn-default btn-squared btn-lang btn-cntotc" data-tolang='cn'>简体</button>
                                            </div>
                                        </li>
                                        </if>
                                    </if>
                                </ul>
                            </div>

                        <!-- Footer -->
                            <footer id="footer">
                                <if value="$ui['link_ok']">
                                    <ul class="breadcrumb p-0 link-img m-0">
                                        <li class='breadcrumb-item'>{$ui.link_txt} :</li>
                                        <tag action='link.list'>
                                            <li class='breadcrumb-item <if value="$ui[split]">split</if>'>
                                                <a href="{$v.weburl}" title="{$v.webname}" {$v.nofollow} target="_blank">
                                                    <if value="$v.link_type eq 1">
                                                        <img src="{$v.weblogo}" alt="{$v.webname}" height='40'>
                                                    <else/>
                                                        <span>{$v.webname}</span>
                                                    </if>
                                                </a>
                                            </li>
                                        </tag>
                                    </ul>
                                </if>
                                <div class="foot_icon">
                                    <if value="$c['met_footright']">
                                        <span>{$c.met_footright}</span>
                                    </if>
                                    <if value="$c['met_footaddress']">
                                        <span>{$c.met_footaddress}</span>
                                    </if>
                                    <if value="$c['met_foottel']">
                                        <span>{$c.met_foottel}</span>
                                    </if>
                                    <if value="$c['met_foottext']">
                                        <span class="hide">{$c.met_foottext}</span>
                                    </if>
                                    <if value="$c['met_footother']">
                                        <div>
                                            {$c.met_footother}
                                        </div>
                                    </if>
                                </div>
                                <div class="powered_by_metinfo">
                                    {$c.met_agents_copyright_foot}
                                </div>
                            </footer>
                    </div>
                </div>
            </div>
        </div>
    </div>
</div>