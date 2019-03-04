<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    $fullpage_ok = $data['classnow']== 10001?"ok":" ";
    $ny_mark_ok = $data['classnow'] != 10001 && $ui['ny_mark_bg']?"ny-mark":" ";
?>
<if value="$ui['head_loader_ok']">
    <div class="$uicss-loader-box <if value='$ui[head_loader_bg_type]'>loader-bgcolor<else/>loader-bgpic</if>"> 
        <div class="loader {$ui.head_loader_type}">
          <div>
            <div>
              <div>
                <div>
                  <div>
                    <div></div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div class="loader-txt">{$ui.head_loader_txt}</div>
    </div>
</if>
<if value="$ui['top_ok']">
    <div class="$uicss-btn-top met-scroll-top {$fullpage_ok}" m-id='{$ui.mid}' hidden>
        <button class="active" number="1" m-type="nocontent">
            <i class="icon fa-play" aria-hidden="true"></i>
        </button>
    </div>
</if>
<div class="mp-pusher $uicss-pusher" id="mp-pusher" data-fullpage="{$fullpage_ok}" data-mask="{$ui.Mask}" data-scroll="{$ui.scroll_type}">
    <if value="($data[classnow] neq 10001&&!$data[id])||$data[module] eq 1">
        <h1 hidden>{$data.name}</h1>
        <if value="$data[classtype] neq 1">
            <tag action="category" type="current" cid="$data['class1']">
                <h2 hidden>{$m.name}</h2>
            </tag>
        </if>
        <else/>
        <if value="!$data[id]">
            <h1 hidden>{$data.name}</h1>
        </if>
    </if>
    <!-- 侧边导航开始 -->
    <nav id="mp-menu" class="mp-menu $uicss-menu $uicss-dustu">
        <div class="mp-level menu--dustu">
            <h2 class="display all-title menu__item <if value="$data['classnow'] eq 10001">active</if>">
                <a class="menu__item-name" href="{$c.index_url}" title="{$word.home}">
                    {$word.home}
                </a>
            </h2>
            <!--一级导航开始 -->
            <ul class="right-head menu--dustu">
                <tag action='category' type='head' class='active' hide ="$ui['hide']">
                    <if value="$ui['navdropdown_ok'] && $m['sub']">
                        <li class="arrow-right menu__item {$m.class}">
                            <a class="display menu__item-name" href="#">
                                <span>{$m.name}</span>
                                <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                            </a>
                            <if value="$ui['nav_desc_ok']">
                                <p class="description menu__item-label">
<?php
    $lengs = strlen($m['description']) > $ui['nav_desc_num']?"...":" ";
?>
{$m.description|met_substr:0,$ui['nav_desc_num']}{$lengs}
                                </p>
                            </if>
                            <div class="mp-level menu--dustu">
                                <h2 class="display all-title menu__item {$m.class}">
                                    <a class="menu__item-name" href="{$m.url}" title="{$m.name}" {$m.urlnew} <if value="$m['nofollow']">rel="nofollow"</if>>
                                        <span>{$m.name}</span>
                                    </a>
                                </h2>
                                
                                <!--二级导航开始 -->
                                <ul class="menu--dustu">
                                    <li class="arrow-left menu__item">
                                        <a class="mp-back display menu__item-name" href="#" title="{$ui.nav_back}">
                                            <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                            <span>{$ui.nav_back}</span>
                                        </a>
                                    </li>
                                    <tag action='category' cid="$m['id']" type='son' class='active'>
                                        <if value="$m['sub']">
                                            <li class="arrow-left menu__item {$m.class}">
                                                <a class="display menu__item-name" href="#">
                                                    <span>{$m.name}</span>
                                                    <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                                </a>
                                                <if value="$ui['nav_desc_ok']">
                                                    <p class="description menu__item-label">
<?php
    $lengs = strlen($m['description']) > $ui['nav_desc_num']?"...":" ";
?>
{$m.description|met_substr:0,$ui['nav_desc_num']}{$lengs}
                                                    </p>
                                                </if>
                                                <div class="mp-level menu--dustu ">
                                                    <h2 class="display all-title menu__item {$m.class}">
                                                        <a class="menu__item-name" href="{$m.url}" title="{$m.name}" {$m.urlnew} <if value="$m['nofollow']">rel="nofollow"</if>>
                                                            <span>{$m.name}</span>
                                                        </a>
                                                    </h2>
                                                    <!--三级导航开始 -->
                                                    <ul class="menu--dustu">
                                                        <li class="arrow-left menu__item">
                                                            <a class="mp-back display menu__item-name" href="#" title="{$ui.nav_back}">
                                                                <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                                                <span>{$ui.nav_back}</span>
                                                            </a>
                                                        </li>
                                                        <tag action='category' cid="$m['id']" type='son' class='active'>
                                                            <li class="arrow-left menu__item {$m.class}">
                                                                <a class="display menu__item-name" href="{$m.url}" title="{$m.name}" {$m.urlnew} <if value="$m['nofollow']">rel="nofollow"</if>>
                                                                    <span>{$m.name}</span>
                                                                </a>
                                                                <if value="$ui['nav_desc_ok']">
                                                                    <p class="description menu__item-label">
<?php
    $lengs = strlen($m['description']) > $ui['nav_desc_num']?"...":" ";
?>
{$m.description|met_substr:0,$ui['nav_desc_num']}{$lengs}
                                                                    </p>
                                                                </if>
                                                            </li>
                                                        </tag>
                                                    </ul>
                                                    <!--三级导航结束 -->
                                                </div>
                                            </li>
                                        <else/>
                                            <li class="arrow-left menu__item {$m.class}">
                                                <a class="display menu__item-name" href="{$m.url}" title="{$m.name}" {$m.urlnew}
                                                <if value="$m['nofollow']">rel="nofollow"</if>>
                                                    <span>{$m.name}</span>
                                                </a>
                                                <if value="$ui['nav_desc_ok']">
                                                    <p class="description menu__item-label">
<?php
    $lengs = strlen($m['description']) > $ui['nav_desc_num']?"...":" ";
?>
{$m.description|met_substr:0,$ui['nav_desc_num']}{$lengs}
                                                    </p>
                                                </if>
                                            </li>
                                        </if>
                                        <!-- 一级导航结束 -->
                                    </tag>
                                </ul>
                                <!--二级导航结束 -->
                            </div>
                        </li>
                    <else/>
                        <li class="arrow-right menu__item {$m.class}">
                            <a class="display menu__item-name" href="{$m.url}" title="{$m.name}" {$m.urlnew} <if value="$m['nofollow']">rel="nofollow"</if>>
                                <span>{$m.name}</span>
                            </a>
                            <if value="$ui['nav_desc_ok']">
                                <p class="description menu__item-label">
<?php
    $lengs = strlen($m['description']) > $ui['nav_desc_num']?"...":" ";
?>
{$m.description|met_substr:0,$ui['nav_desc_num']}{$lengs}
                                </p>
                            </if>
                        </li>
                    </if>
                </tag>
                <!-- 多语言开始-->
                <if value="$c['met_lang_mark'] && $ui['lang_ok']">
                    <li class="met-langlist arrow-right menu__item ">
                        <lang>
                            <if value="$data['lang'] eq $v['mark']">
                                <a href="#" title="{$m.name}" class="display menu__item-name">
                                    <if value="$ui['langlist_icon_ok']">
                                        <img src="{$v.flag}" alt="{$v.name}" style="max-width:100%;">
                                    </if>
                                    <span >{$v.name}</span>
                                    <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                    <!-- <i class="icon fa-angle-right" aria-hidden="true"></i> -->
                                </a>
                            </if>
                        </lang>
                        <div class="mp-level menu--dustu">
                            <ul class="menu--dustu">
                                <li class="arrow-left menu__item">
                                    <a class="mp-back display menu__item-name" href="#" title="{$ui.nav_back}">
                                        <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                        <span>{$ui.nav_back}</span>
                                    </a>
                                </li>
                                <lang>
                                    <li class="arrow-left menu__item ">
                                        <a href="{$v.met_weburl}" title="{$v.name}" class='display menu__item-name'>
                                            <if value="$ui['langlist_icon_ok']">
                                                <img src="{$v.flag}" alt="{$v.name}" style="max-width:100%;">
                                            </if>
                                            <span>{$v.name}</span>
                                        </a>
                                    </li>
                                </lang>
                            </ul>
                        </div>
                    </li>
                </if>
                <!-- 多语言结束-->
                <!-- 简繁体开始-->
                <if value="$c['met_ch_lang'] && $ui['simplified_ok']">
                    <if value="$data[lang] eq cn">
                        <li class="arrow-right menu__item langjf-box" m-id="lang" m-type="lang">
                            <a class="link display menu__item-name">
                                <span class="btn-cntotc" data-tolang='tc'>繁体</span>
                    <elseif value="$data[lang] eq zh"/>
                                <span class="btn-cntotc"  data-tolang='cn'>简体</span>
                            </a>
                        </li>
                    </if>
                            </a>
                        </li>
                </if>
                <!-- 简繁体结束-->
                <!-- 登录 注册开始 -->
                <if value="$c[met_member_register] && $ui['member_ok']">
                    <li class="arrow-right menu__item dl-box">
                        <a class="display menu__item-name" href="{$m.url}" title="{$m.name}">
                            <span>{$ui.member_txt}</span>
                            <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                        </a>
                        <div class="mp-level menu--dustu" id='met-head-user-collapse' m-id='member' m-type='member'>
                        <if value="$user">
                            <if value="$c['shopv2_open']">
                            <!-- 开启商城登录后开始 -->
                                <h2 class="display all-title menu__item {$m.class}">
                                    <a class="menu__item-name" href="{$url.shop_profile}" title="{$user.username}" >
                                        <span>
                                            <img src="{$user.head}" alt="{$user.username}"/>
                                        </span>
                                    </a>
                                </h2>
                                <ul class="ACC menu--dustu" m-id="member" m-type="member">
                                    <!-- 开启商城后登录个人中心开始 -->

                                    <li class="arrow-right menu__item">
                                        <a href="javascript:;" class="display menu__item-name">
                                            <span class="">
                                                {$user.username}
                                            </span>
                                            <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                        </a>
                                        <div class="mp-level menu--dustu" role="menu">
                                            <ul class="menu--dustu">
                                                <li class="arrow-left menu__item">
                                                    <a class="mp-back display menu__item-name" href="#" title="{$ui.nav_back}">
                                                        <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                                        <span>{$ui.nav_back}</span>
                                                    </a>
                                                </li>
                                                s<li role="presentation" class="arrow-left menu__item">
                                                    <a href="{$url.shop_profile}" class="display menu__item-name" role="menuitem">
                                                        <i class="icon wb-user" aria-hidden="true"></i>
                                                        <span>
                                                            {$word.app_shop_personal}
                                                        </span>
                                                    </a>
                                                </li>
                                                <li role="presentation" class="arrow-left menu__item">
                                                    <a href="{$url.shop_order}" class="display menu__item-name" role="menuitem">
                                                        <i class="icon wb-order" aria-hidden="true"></i>
                                                        <span>{$word.app_shop_myorder}</span>
                                                    </a>
                                                </li>
                                                <li role="presentation" class="arrow-left menu__item">
                                                    <a href="{$url.shop_favorite}" class="display menu__item-name" role="menuitem">
                                                        <i class="icon wb-heart" aria-hidden="true"></i>
                                                        <span>{$word.app_shop_myfavorite}</span>
                                                    </a>
                                                </li>
                                                <li role="presentation" class="arrow-left menu__item">
                                                    <a href="{$url.shop_discount}" class="display menu__item-name" role="menuitem">
                                                        <i class="icon wb-bookmark" aria-hidden="true"></i>
                                                        <span>{$word.app_shop_mydiscount}</span>
                                                    </a>
                                                </li>
                                                <li role="presentation" class="arrow-left menu__item">
                                                    <a href="{$url.shop_member_base}&nojump=1" class="display menu__item-name" target="_blank" role="menuitem">
                                                        <i class="icon wb-settings" aria-hidden="true"></i>
                                                        <span>{$word.app_shop_settings}</span>
                                                    </a>
                                                </li>
                                                <li role="presentation" class="arrow-left menu__item">
                                                    <a href="{$url.shop_member_login_out}" class="display menu__item-name" role="menuitem">
                                                        <i class="icon wb-power" aria-hidden="true"></i>
                                                        <span>{$word.app_shop_out}</span>
                                                    </a>
                                                </li>
                                            </ul>
                                        </div>
                                    </li>
                                    <!-- 开启商城后登录个人中心结束 -->
                                    <!-- 开启商城后登录购物车开始 -->
                                    <li class="arrow-right menu__item">
                                        <a href="javascript:;" class="display menu__item-name topcart-btn" title="{$word.app_shop_cart}">
                                            <i class=" icon pe-shopbag" aria-hidden="true"></i>
                                            <span class="">
                                                {$word.app_shop_cart}
                                            </span>
                                            <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                        </a>
                                        <div class="mp-level menu--dustu">
                                            <ul class="menu--dustu spCart" role="menu">
                                                <li class="dropdown-menu-header">
                                                    <h5 class="spCart-title">
                                                        {$word.app_shop_cart}
                                                    </h5>
                                                    <span class="label label-round label-danger spCart-num">{$word.app_shop_intotal} <span class="topcart-goodnum"></span> {$word.app_shop_piece}{$word.app_shop_commodity}</span>
                                                </li>
                                                <li class="list-group dropdown-scrollable shop_cart_bg" role="presentation">
                                                    <div data-role="container">
                                                        <div data-role="content" id="topcart-body"></div>
                                                    </div>
                                                </li>
                                                <li class="dropdown-menu-footer p-y-10 p-x-20 shop_cart_btn" role="presentation">
                                                    <a href="{$url.shop_cart}" class="btn btn-squared btn-danger margin-bottom-5 margin-right-10">{$word.app_shop_gosettlement}</a>
                                                </li>
                                                <li class="shop_cart_price">
                                                    <span class=" font-size-18 topcart-total"></span>
                                                </li>
                                            </ul>
                                        </div>
                                    </li>
                                    <if value="$ui[head_top_sc_ok]">
                                        <li class="inline-block text-xs-center vertical-align-middle animation-slide-top">
                                            <div class="vertical-align-middle shopcart-btn login-btn shop-sc">
                                                <a href="javascript:void(0);" onclick="AddFavorite('{$c.met_webname}',location.href)">
                                                    <i class="icon fa-heart-o" aria-hidden="true" style="font-size:16px;"></i>
                                                </a>  
                                            </div>
                                        </li>
                                    </if>
                                    <!-- 开启商城后登录购物车结束 -->
                                </ul>
                                <!-- 开启商城登录后结束 -->
                                <else/>
                                    <!-- 未开商城登录后开始 -->
                                    <h2 class="display all-title menu__item {$m.class}">
                                        <a class="menu__item-name" href="{$url.shop_profile}" title="{$user.username}">
                                            <span>
                                                <img src="{$user.head}" alt="{$user.username}"/>
                                            </span>
                                        </a>
                                    </h2>
                                    <ul class="AQQ  menu--dustu met-head-user" m-id="member" m-type="member">

                                        <li class="arrow-right menu__item">
                                            <a href="javascript:;" class="display menu__item-name">
                                                <span class="">
                                                    {$user.username}
                                                </span>
                                                <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                            </a>
                                            <div class="mp-level menu--dustu">
                                                <ul class="menu--dustu">
                                                    <li role="presentation" class="arrow-left menu__item">
                                                        <a href="{$c.met_weburl}member/basic.php?lang={$_M[lang]}" class="display menu__item-name" title='{$word.memberIndex9}' role="menuitem">
                                                            <i class="icon wb-user" aria-hidden="true"></i>
                                                            <span>{$word.memberIndex9}</span>
                                                        </a>
                                                    </li>
                                                    <li role="presentation" class="arrow-left menu__item ">
                                                        <a href="{$c.met_weburl}member/basic.php?lang={$_M[lang]}&a=dosafety" class="display menu__item-name" title='{$word.accsafe}' role="menuitem">
                                                            <i class="icon wb-lock" aria-hidden="true"></i>
                                                            <span>{$word.accsafe}</span>
                                                        </a>
                                                    </li>
                                                    <li role="presentation" class="arrow-left menu__item ">
                                                        <a href="{$c.met_weburl}member/login.php?lang={$_M[lang]}&a=dologout" class="display menu__item-name" role="menuitem">
                                                            <i class="icon wb-power" aria-hidden="true"></i>
                                                            <span>{$word.memberIndex10}</span>
                                                        </a>
                                                    </li>
                                                </ul>
                                            </div>
                                        </li>
                                    </ul>
                                    <!-- 未开商城登录后开始 -->
                                </if>
                            <else/>
                            <if value="$c['shopv2_open']">
                                <ul class="menu--dustu" m-id="member" m-type="member">
                                        <li class="arrow-left menu__item">
                                            <a class="mp-back display menu__item-name" href="#" title="{$ui.nav_back}">
                                                <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                                <span>{$ui.nav_back}</span>
                                            </a>
                                        </li>
                                        <li class="arrow-right menu__item">
                                            <a href="{$_M[url][site]}member/login.php?lang={$_M[lang]}" class="display menu__item-name">
                                                <i class="icon fa-user" aria-hidden="true"></i>
                                                {$word.login}
                                            </a>
                                        </li>
                                        <li class="arrow-right menu__item">
                                            <a
                                                href="javascript:void(0)"
                                                title="{$word.app_shop_cart}"
                                                class='display menu__item-name'
                                            >
                                                <i class="icon fa-shopping-cart" aria-hidden="true"></i>
                                                {$word.app_shop_cart}
                                                <span class="badge badge-danger up hide topcart-goodnum"></span>
                                                <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                            </a>
                                            <div class="mp-level menu--dustu">
                                                <ul class="menu--dustu spCart" role="menu">
                                                    <li class="dropdown-menu-header">
                                                        <h5 class="spCart-title">
                                                            {$word.app_shop_cart}
                                                        </h5>
                                                        <span class="label label-round label-danger spCart-num">{$word.app_shop_intotal} <span class="topcart-goodnum"></span> {$word.app_shop_piece}{$word.app_shop_commodity}</span>
                                                    </li>
                                                    <li class="list-group dropdown-scrollable shop_cart_bg" role="presentation">
                                                        <div data-role="container">
                                                            <div data-role="content" id="topcart-body"></div>
                                                        </div>
                                                    </li>
                                                    <li class="dropdown-menu-footer p-y-10 p-x-20 shop_cart_btn" role="presentation">
                                                            <a href="{$url.shop_cart}" class="btn btn-squared btn-danger margin-bottom-5 margin-right-10">{$word.app_shop_gosettlement}</a>
                                                    </li>
                                                    <li class="shop_cart_price">
                                                        <span class="font-size-18 topcart-total"></span>
                                                    </li>
                                                </ul>
                                        </div>
                                        </li>
                                            <if value="$ui[head_top_search_ok]">
                                                <div class="vertical-align-middle shopcart-btn login-btn search-index">
                                                    <form method="get" action="{$c.index_url}search/search.php?lang={$_M[lang]}">
                                                        <input type="hidden" name="lang" value="{$_M[lang]}">
                                                        <div class="input-search">
                                                            <button type="submit" class="input-search-btn"><i class="icon wb-search" aria-hidden="true"></i></button>
                                                            <input type="text" class="search" name="searchword" value="" placeholder="{$ui.search}" data-fv-notempty="true" data-fv-message="不能为空">
                                                        </div>
                                                    </form>
                                                </div>
                                            </if>
                                </ul>
                            <else/>
                                <!-- 未登录状态开始 -->
                                 <ul class=" menu--dustu met-head-user met-nav-login" m-id="member" m-type="member">
                                    <li class="arrow-left menu__item">
                                        <a class="mp-back display menu__item-name" href="#" title="{$ui.nav_back}">
                                            <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                            <span>{$ui.nav_back}</span>
                                        </a>
                                    </li>
                                    <li class="arrow-right menu__item">
                                        <a href="{$_M[url][site]}member/login.php?lang={$_M[lang]}" class="btn-outline display menu__item-name">
                                            <span>{$word.login}</span>
                                        </a>
                                    </li>
                                    <li class="arrow-right menu__item">
                                        <a href="{$_M[url][site]}member/register_include.php?lang={$_M[lang]}" class="btn-success display menu__item-name">
                                            <span>{$word.register}</span>
                                        </a>
                                    </li>
                                </ul>
                                <!-- 未登录状态开始 -->
                            </if>
                        </if>
                        </div>
                    </li>
                </if>
            <!-- 登录 注册结束 -->
            </ul>
            <!--一级导航结束 -->
            <div class="right-head-wx">
               <ul>
                    <if value="$ui['footinfo_wx_ok']">
                        <li>
                            <?php $img=strstr($ui['footinfo_wx'],"http"); ?>
                            <a  class="icon style2 fa-weixin" id="met-weixin" data-plugin="webuiPopover" data-trigger="hover" data-animation="pop" data-placement='top' data-width='155' data-padding='0' data-content="<div class='text-xs-center'><if value='$img'><img src='{$ui.footinfo_wx}' alt='{$c.met_webname}' width='150' height='150' id='met-weixin-img'></if></div>"></a>
                        </li>
                    </if>
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
                <if value="$ui['tel_txt']">
                    <div class="tel">{$ui.tel_txt}</div>
                </if>
            </div>
        </div>
        
    </nav>
    <!-- 侧边导航结束 -->
    <!-- 头部功能集合开始 -->
    <div id="menu" class="$uicss-top-head $uicss-dustu {$ny_mark_ok}"  m-id='{$ui.mid}' m-type='head_nav'>
        <div class="container">
            <!-- LOGO开始-->
            <div class="head-logo" m-id='{$ui.mid}'>
                <a href="{$c.index_url}" class="met-logo vertical-align navbar-logo" title="{$c.met_webname}">
                    <div class="vertical-align-middle">
                        <img src="{$c.met_logo}" alt="{$c.met_webname}" title="{$c.met_webname}">
                    </div>
                </a>
            </div>
            <!-- LOGO结束-->
            <!-- 菜单按键开始-->
            <a class="more pull-right">
                <svg class="ham ham6 $uicss-ham"  id="trigger" class="menu-trigger" viewBox="0 0 100 100" width="56">
                  <path class="line top" d="m 30,33 h 40 c 13.100415,0 14.380204,31.80258 6.899646,33.421777 -24.612039,5.327373 9.016154,-52.337577 -12.75751,-30.563913 l -28.284272,28.284272" />
                  <path class="line middle" d="m 70,50 c 0,0 -32.213436,0 -40,0 -7.786564,0 -6.428571,-4.640244 -6.428571,-8.571429 0,-5.895471 6.073743,-11.783399 12.286435,-5.570707 6.212692,6.212692 28.284272,28.284272 28.284272,28.284272" />
                  <path class="line bottom" d="m 69.575405,67.073826 h -40 c -13.100415,0 -14.380204,-31.80258 -6.899646,-33.421777 24.612039,-5.327373 -9.016154,52.337577 12.75751,30.563913 l 28.284272,-28.284272" />
                </svg>
            </a>
            <!-- 菜单按键结束-->
            <!-- 首页引导菜单开始-->
            <div class="pull-right top_ydhead nav-tabs-horizontal nav-tabs-inverse nav-tabs-animate $uicss-nav-tabs">
                <ul class="nav-top menu--dustu nav nav-tabs nav-tabs-solid flexs flex-center">
                    <if value="$data['classnow'] neq 10001">
                        <tag action='category' cid="$data['releclass1']" class="active" type="son">
                            <li role='presentation' class='menu__item {$m.class}'>
                                <a class='menu__item-name' href='{$m.url}'>{$m.name}</a>
                            </li>
                        </tag>
                    </if>
                </ul>
            </div>
            <!-- 首页引导菜单结束-->
        </div>
    </div>
    <!-- 头部功能集合结束 -->
    <!-- 主要内容开始 -->
    <div id="fullpage" class="$uicss-fullpage">
