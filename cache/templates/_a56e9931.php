<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    $fullpage_ok = $data['classnow']== 10001?"ok":" ";
    $ny_mark_ok = $data['classnow'] != 10001 && $ui['ny_mark_bg']?"ny-mark":" ";
?>
    <?php if($ui['head_loader_ok']){ ?>
    <div class="$uicss-loader-box     <?php if($ui[head_loader_bg_type]){ ?>loader-bgcolor<?php }else{ ?>loader-bgpic<?php } ?>"> 
        <div class="loader <?php echo $ui['head_loader_type'];?>">
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
        <div class="loader-txt"><?php echo $ui['head_loader_txt'];?></div>
    </div>
<?php } ?>
    <?php if($ui['top_ok']){ ?>
    <div class="$uicss-btn-top met-scroll-top <?php echo $fullpage_ok;?>" m-id='<?php echo $ui['mid'];?>' hidden>
        <button class="active" number="1" m-type="nocontent">
            <i class="icon fa-play" aria-hidden="true"></i>
        </button>
    </div>
<?php } ?>
<div class="mp-pusher $uicss-pusher" id="mp-pusher" data-fullpage="<?php echo $fullpage_ok;?>" data-mask="<?php echo $ui['Mask'];?>" data-scroll="<?php echo $ui['scroll_type'];?>">
        <?php if(($data[classnow]<>10001&&!$data[id])||$data[module]==1){ ?>
        <h1 hidden><?php echo $data['name'];?></h1>
            <?php if($data[classtype]<>1){ ?>
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
                <h2 hidden><?php echo $m['name'];?></h2>
            <?php endforeach;?>
        <?php } ?>
        <?php }else{ ?>
            <?php if(!$data[id]){ ?>
            <h1 hidden><?php echo $data['name'];?></h1>
        <?php } ?>
    <?php } ?>
    <!-- 侧边导航开始 -->
    <nav id="mp-menu" class="mp-menu $uicss-menu $uicss-dustu">
        <div class="mp-level menu--dustu">
            <h2 class="display all-title menu__item     <?php if($data['classnow']==10001){ ?>active<?php } ?>">
                <a class="menu__item-name" href="<?php echo $c['index_url'];?>" title="<?php echo $word['home'];?>">
                    <?php echo $word['home'];?>
                </a>
            </h2>
            <!--一级导航开始 -->
            <ul class="right-head menu--dustu">
                <?php
    $type=strtolower(trim('head'));
    $cid=0;
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
            $m['class']="active";
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
                        <?php if($ui['navdropdown_ok'] && $m['sub']){ ?>
                        <li class="arrow-right menu__item <?php echo $m['class'];?>">
                            <a class="display menu__item-name" href="#">
                                <span><?php echo $m['name'];?></span>
                                <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                            </a>
                                <?php if($ui['nav_desc_ok']){ ?>
                                <p class="description menu__item-label">
<?php
    $lengs = strlen($m['description']) > $ui['nav_desc_num']?"...":" ";
?>
<?php echo met_substr($m['description'],0,$ui['nav_desc_num']);?><?php echo $lengs;?>
                                </p>
                            <?php } ?>
                            <div class="mp-level menu--dustu">
                                <h2 class="display all-title menu__item <?php echo $m['class'];?>">
                                    <a class="menu__item-name" href="<?php echo $m['url'];?>" title="<?php echo $m['name'];?>" <?php echo $m['urlnew'];?>     <?php if($m['nofollow']){ ?>rel="nofollow"<?php } ?>>
                                        <span><?php echo $m['name'];?></span>
                                    </a>
                                </h2>
                                
                                <!--二级导航开始 -->
                                <ul class="menu--dustu">
                                    <li class="arrow-left menu__item">
                                        <a class="mp-back display menu__item-name" href="#" title="<?php echo $ui['nav_back'];?>">
                                            <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                            <span><?php echo $ui['nav_back'];?></span>
                                        </a>
                                    </li>
                                    <?php
    $type=strtolower(trim('son'));
    $cid=$m['id'];
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
            $m['class']="active";
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
                                            <?php if($m['sub']){ ?>
                                            <li class="arrow-left menu__item <?php echo $m['class'];?>">
                                                <a class="display menu__item-name" href="#">
                                                    <span><?php echo $m['name'];?></span>
                                                    <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                                </a>
                                                    <?php if($ui['nav_desc_ok']){ ?>
                                                    <p class="description menu__item-label">
<?php
    $lengs = strlen($m['description']) > $ui['nav_desc_num']?"...":" ";
?>
<?php echo met_substr($m['description'],0,$ui['nav_desc_num']);?><?php echo $lengs;?>
                                                    </p>
                                                <?php } ?>
                                                <div class="mp-level menu--dustu ">
                                                    <h2 class="display all-title menu__item <?php echo $m['class'];?>">
                                                        <a class="menu__item-name" href="<?php echo $m['url'];?>" title="<?php echo $m['name'];?>" <?php echo $m['urlnew'];?>     <?php if($m['nofollow']){ ?>rel="nofollow"<?php } ?>>
                                                            <span><?php echo $m['name'];?></span>
                                                        </a>
                                                    </h2>
                                                    <!--三级导航开始 -->
                                                    <ul class="menu--dustu">
                                                        <li class="arrow-left menu__item">
                                                            <a class="mp-back display menu__item-name" href="#" title="<?php echo $ui['nav_back'];?>">
                                                                <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                                                <span><?php echo $ui['nav_back'];?></span>
                                                            </a>
                                                        </li>
                                                        <?php
    $type=strtolower(trim('son'));
    $cid=$m['id'];
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
            $m['class']="active";
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
                                                            <li class="arrow-left menu__item <?php echo $m['class'];?>">
                                                                <a class="display menu__item-name" href="<?php echo $m['url'];?>" title="<?php echo $m['name'];?>" <?php echo $m['urlnew'];?>     <?php if($m['nofollow']){ ?>rel="nofollow"<?php } ?>>
                                                                    <span><?php echo $m['name'];?></span>
                                                                </a>
                                                                    <?php if($ui['nav_desc_ok']){ ?>
                                                                    <p class="description menu__item-label">
<?php
    $lengs = strlen($m['description']) > $ui['nav_desc_num']?"...":" ";
?>
<?php echo met_substr($m['description'],0,$ui['nav_desc_num']);?><?php echo $lengs;?>
                                                                    </p>
                                                                <?php } ?>
                                                            </li>
                                                        <?php endforeach;?>
                                                    </ul>
                                                    <!--三级导航结束 -->
                                                </div>
                                            </li>
                                        <?php }else{ ?>
                                            <li class="arrow-left menu__item <?php echo $m['class'];?>">
                                                <a class="display menu__item-name" href="<?php echo $m['url'];?>" title="<?php echo $m['name'];?>" <?php echo $m['urlnew'];?>
                                                    <?php if($m['nofollow']){ ?>rel="nofollow"<?php } ?>>
                                                    <span><?php echo $m['name'];?></span>
                                                </a>
                                                    <?php if($ui['nav_desc_ok']){ ?>
                                                    <p class="description menu__item-label">
<?php
    $lengs = strlen($m['description']) > $ui['nav_desc_num']?"...":" ";
?>
<?php echo met_substr($m['description'],0,$ui['nav_desc_num']);?><?php echo $lengs;?>
                                                    </p>
                                                <?php } ?>
                                            </li>
                                        <?php } ?>
                                        <!-- 一级导航结束 -->
                                    <?php endforeach;?>
                                </ul>
                                <!--二级导航结束 -->
                            </div>
                        </li>
                    <?php }else{ ?>
                        <li class="arrow-right menu__item <?php echo $m['class'];?>">
                            <a class="display menu__item-name" href="<?php echo $m['url'];?>" title="<?php echo $m['name'];?>" <?php echo $m['urlnew'];?>     <?php if($m['nofollow']){ ?>rel="nofollow"<?php } ?>>
                                <span><?php echo $m['name'];?></span>
                            </a>
                                <?php if($ui['nav_desc_ok']){ ?>
                                <p class="description menu__item-label">
<?php
    $lengs = strlen($m['description']) > $ui['nav_desc_num']?"...":" ";
?>
<?php echo met_substr($m['description'],0,$ui['nav_desc_num']);?><?php echo $lengs;?>
                                </p>
                            <?php } ?>
                        </li>
                    <?php } ?>
                <?php endforeach;?>
                <!-- 多语言开始-->
                    <?php if($c['met_lang_mark'] && $ui['lang_ok']){ ?>
                    <li class="met-langlist arrow-right menu__item ">
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
                                <a href="#" title="<?php echo $m['name'];?>" class="display menu__item-name">
                                        <?php if($ui['langlist_icon_ok']){ ?>
                                        <img src="<?php echo $v['flag'];?>" alt="<?php echo $v['name'];?>" style="max-width:100%;">
                                    <?php } ?>
                                    <span ><?php echo $v['name'];?></span>
                                    <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                    <!-- <i class="icon fa-angle-right" aria-hidden="true"></i> -->
                                </a>
                            <?php } ?>
                        <?php endforeach;?>
                        <div class="mp-level menu--dustu">
                            <ul class="menu--dustu">
                                <li class="arrow-left menu__item">
                                    <a class="mp-back display menu__item-name" href="#" title="<?php echo $ui['nav_back'];?>">
                                        <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                        <span><?php echo $ui['nav_back'];?></span>
                                    </a>
                                </li>
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
                                    <li class="arrow-left menu__item ">
                                        <a href="<?php echo $v['met_weburl'];?>" title="<?php echo $v['name'];?>" class='display menu__item-name'>
                                                <?php if($ui['langlist_icon_ok']){ ?>
                                                <img src="<?php echo $v['flag'];?>" alt="<?php echo $v['name'];?>" style="max-width:100%;">
                                            <?php } ?>
                                            <span><?php echo $v['name'];?></span>
                                        </a>
                                    </li>
                                <?php endforeach;?>
                            </ul>
                        </div>
                    </li>
                <?php } ?>
                <!-- 多语言结束-->
                <!-- 简繁体开始-->
                    <?php if($c['met_ch_lang'] && $ui['simplified_ok']){ ?>
                        <?php if($data[lang]==cn){ ?>
                        <li class="arrow-right menu__item langjf-box" m-id="lang" m-type="lang">
                            <a class="link display menu__item-name">
                                <span class="btn-cntotc" data-tolang='tc'>繁体</span>
                    <?php }else if($data[lang]==zh){ ?>
                                <span class="btn-cntotc"  data-tolang='cn'>简体</span>
                            </a>
                        </li>
                    <?php } ?>
                            </a>
                        </li>
                <?php } ?>
                <!-- 简繁体结束-->
                <!-- 登录 注册开始 -->
                    <?php if($c[met_member_register] && $ui['member_ok']){ ?>
                    <li class="arrow-right menu__item dl-box">
                        <a class="display menu__item-name" href="<?php echo $m['url'];?>" title="<?php echo $m['name'];?>">
                            <span><?php echo $ui['member_txt'];?></span>
                            <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                        </a>
                        <div class="mp-level menu--dustu" id='met-head-user-collapse' m-id='member' m-type='member'>
                            <?php if($user){ ?>
                                <?php if($c['shopv2_open']){ ?>
                            <!-- 开启商城登录后开始 -->
                                <h2 class="display all-title menu__item <?php echo $m['class'];?>">
                                    <a class="menu__item-name" href="<?php echo $url['shop_profile'];?>" title="<?php echo $user['username'];?>" >
                                        <span>
                                            <img src="<?php echo $user['head'];?>" alt="<?php echo $user['username'];?>"/>
                                        </span>
                                    </a>
                                </h2>
                                <ul class="ACC menu--dustu" m-id="member" m-type="member">
                                    <!-- 开启商城后登录个人中心开始 -->

                                    <li class="arrow-right menu__item">
                                        <a href="javascript:;" class="display menu__item-name">
                                            <span class="">
                                                <?php echo $user['username'];?>
                                            </span>
                                            <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                        </a>
                                        <div class="mp-level menu--dustu" role="menu">
                                            <ul class="menu--dustu">
                                                <li class="arrow-left menu__item">
                                                    <a class="mp-back display menu__item-name" href="#" title="<?php echo $ui['nav_back'];?>">
                                                        <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                                        <span><?php echo $ui['nav_back'];?></span>
                                                    </a>
                                                </li>
                                                s<li role="presentation" class="arrow-left menu__item">
                                                    <a href="<?php echo $url['shop_profile'];?>" class="display menu__item-name" role="menuitem">
                                                        <i class="icon wb-user" aria-hidden="true"></i>
                                                        <span>
                                                            <?php echo $word['app_shop_personal'];?>
                                                        </span>
                                                    </a>
                                                </li>
                                                <li role="presentation" class="arrow-left menu__item">
                                                    <a href="<?php echo $url['shop_order'];?>" class="display menu__item-name" role="menuitem">
                                                        <i class="icon wb-order" aria-hidden="true"></i>
                                                        <span><?php echo $word['app_shop_myorder'];?></span>
                                                    </a>
                                                </li>
                                                <li role="presentation" class="arrow-left menu__item">
                                                    <a href="<?php echo $url['shop_favorite'];?>" class="display menu__item-name" role="menuitem">
                                                        <i class="icon wb-heart" aria-hidden="true"></i>
                                                        <span><?php echo $word['app_shop_myfavorite'];?></span>
                                                    </a>
                                                </li>
                                                <li role="presentation" class="arrow-left menu__item">
                                                    <a href="<?php echo $url['shop_discount'];?>" class="display menu__item-name" role="menuitem">
                                                        <i class="icon wb-bookmark" aria-hidden="true"></i>
                                                        <span><?php echo $word['app_shop_mydiscount'];?></span>
                                                    </a>
                                                </li>
                                                <li role="presentation" class="arrow-left menu__item">
                                                    <a href="<?php echo $url['shop_member_base'];?>&nojump=1" class="display menu__item-name" target="_blank" role="menuitem">
                                                        <i class="icon wb-settings" aria-hidden="true"></i>
                                                        <span><?php echo $word['app_shop_settings'];?></span>
                                                    </a>
                                                </li>
                                                <li role="presentation" class="arrow-left menu__item">
                                                    <a href="<?php echo $url['shop_member_login_out'];?>" class="display menu__item-name" role="menuitem">
                                                        <i class="icon wb-power" aria-hidden="true"></i>
                                                        <span><?php echo $word['app_shop_out'];?></span>
                                                    </a>
                                                </li>
                                            </ul>
                                        </div>
                                    </li>
                                    <!-- 开启商城后登录个人中心结束 -->
                                    <!-- 开启商城后登录购物车开始 -->
                                    <li class="arrow-right menu__item">
                                        <a href="javascript:;" class="display menu__item-name topcart-btn" title="<?php echo $word['app_shop_cart'];?>">
                                            <i class=" icon pe-shopbag" aria-hidden="true"></i>
                                            <span class="">
                                                <?php echo $word['app_shop_cart'];?>
                                            </span>
                                            <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                        </a>
                                        <div class="mp-level menu--dustu">
                                            <ul class="menu--dustu spCart" role="menu">
                                                <li class="dropdown-menu-header">
                                                    <h5 class="spCart-title">
                                                        <?php echo $word['app_shop_cart'];?>
                                                    </h5>
                                                    <span class="label label-round label-danger spCart-num"><?php echo $word['app_shop_intotal'];?> <span class="topcart-goodnum"></span> <?php echo $word['app_shop_piece'];?><?php echo $word['app_shop_commodity'];?></span>
                                                </li>
                                                <li class="list-group dropdown-scrollable shop_cart_bg" role="presentation">
                                                    <div data-role="container">
                                                        <div data-role="content" id="topcart-body"></div>
                                                    </div>
                                                </li>
                                                <li class="dropdown-menu-footer p-y-10 p-x-20 shop_cart_btn" role="presentation">
                                                    <a href="<?php echo $url['shop_cart'];?>" class="btn btn-squared btn-danger margin-bottom-5 margin-right-10"><?php echo $word['app_shop_gosettlement'];?></a>
                                                </li>
                                                <li class="shop_cart_price">
                                                    <span class=" font-size-18 topcart-total"></span>
                                                </li>
                                            </ul>
                                        </div>
                                    </li>
                                        <?php if($ui[head_top_sc_ok]){ ?>
                                        <li class="inline-block text-xs-center vertical-align-middle animation-slide-top">
                                            <div class="vertical-align-middle shopcart-btn login-btn shop-sc">
                                                <a href="javascript:void(0);" onclick="AddFavorite('<?php echo $c['met_webname'];?>',location.href)">
                                                    <i class="icon fa-heart-o" aria-hidden="true" style="font-size:16px;"></i>
                                                </a>  
                                            </div>
                                        </li>
                                    <?php } ?>
                                    <!-- 开启商城后登录购物车结束 -->
                                </ul>
                                <!-- 开启商城登录后结束 -->
                                <?php }else{ ?>
                                    <!-- 未开商城登录后开始 -->
                                    <h2 class="display all-title menu__item <?php echo $m['class'];?>">
                                        <a class="menu__item-name" href="<?php echo $url['shop_profile'];?>" title="<?php echo $user['username'];?>">
                                            <span>
                                                <img src="<?php echo $user['head'];?>" alt="<?php echo $user['username'];?>"/>
                                            </span>
                                        </a>
                                    </h2>
                                    <ul class="AQQ  menu--dustu met-head-user" m-id="member" m-type="member">

                                        <li class="arrow-right menu__item">
                                            <a href="javascript:;" class="display menu__item-name">
                                                <span class="">
                                                    <?php echo $user['username'];?>
                                                </span>
                                                <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                            </a>
                                            <div class="mp-level menu--dustu">
                                                <ul class="menu--dustu">
                                                    <li role="presentation" class="arrow-left menu__item">
                                                        <a href="<?php echo $c['met_weburl'];?>member/basic.php?lang=<?php echo $_M[lang];?>" class="display menu__item-name" title='<?php echo $word['memberIndex9'];?>' role="menuitem">
                                                            <i class="icon wb-user" aria-hidden="true"></i>
                                                            <span><?php echo $word['memberIndex9'];?></span>
                                                        </a>
                                                    </li>
                                                    <li role="presentation" class="arrow-left menu__item ">
                                                        <a href="<?php echo $c['met_weburl'];?>member/basic.php?lang=<?php echo $_M[lang];?>&a=dosafety" class="display menu__item-name" title='<?php echo $word['accsafe'];?>' role="menuitem">
                                                            <i class="icon wb-lock" aria-hidden="true"></i>
                                                            <span><?php echo $word['accsafe'];?></span>
                                                        </a>
                                                    </li>
                                                    <li role="presentation" class="arrow-left menu__item ">
                                                        <a href="<?php echo $c['met_weburl'];?>member/login.php?lang=<?php echo $_M[lang];?>&a=dologout" class="display menu__item-name" role="menuitem">
                                                            <i class="icon wb-power" aria-hidden="true"></i>
                                                            <span><?php echo $word['memberIndex10'];?></span>
                                                        </a>
                                                    </li>
                                                </ul>
                                            </div>
                                        </li>
                                    </ul>
                                    <!-- 未开商城登录后开始 -->
                                <?php } ?>
                            <?php }else{ ?>
                                <?php if($c['shopv2_open']){ ?>
                                <ul class="menu--dustu" m-id="member" m-type="member">
                                        <li class="arrow-left menu__item">
                                            <a class="mp-back display menu__item-name" href="#" title="<?php echo $ui['nav_back'];?>">
                                                <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                                <span><?php echo $ui['nav_back'];?></span>
                                            </a>
                                        </li>
                                        <li class="arrow-right menu__item">
                                            <a href="<?php echo $_M[url][site];?>member/login.php?lang=<?php echo $_M[lang];?>" class="display menu__item-name">
                                                <i class="icon fa-user" aria-hidden="true"></i>
                                                <?php echo $word['login'];?>
                                            </a>
                                        </li>
                                        <li class="arrow-right menu__item">
                                            <a
                                                href="javascript:void(0)"
                                                title="<?php echo $word['app_shop_cart'];?>"
                                                class='display menu__item-name'
                                            >
                                                <i class="icon fa-shopping-cart" aria-hidden="true"></i>
                                                <?php echo $word['app_shop_cart'];?>
                                                <span class="badge badge-danger up hide topcart-goodnum"></span>
                                                <i class="icon fa-angle-right nav-angle-right" aria-hidden="true"></i>
                                            </a>
                                            <div class="mp-level menu--dustu">
                                                <ul class="menu--dustu spCart" role="menu">
                                                    <li class="dropdown-menu-header">
                                                        <h5 class="spCart-title">
                                                            <?php echo $word['app_shop_cart'];?>
                                                        </h5>
                                                        <span class="label label-round label-danger spCart-num"><?php echo $word['app_shop_intotal'];?> <span class="topcart-goodnum"></span> <?php echo $word['app_shop_piece'];?><?php echo $word['app_shop_commodity'];?></span>
                                                    </li>
                                                    <li class="list-group dropdown-scrollable shop_cart_bg" role="presentation">
                                                        <div data-role="container">
                                                            <div data-role="content" id="topcart-body"></div>
                                                        </div>
                                                    </li>
                                                    <li class="dropdown-menu-footer p-y-10 p-x-20 shop_cart_btn" role="presentation">
                                                            <a href="<?php echo $url['shop_cart'];?>" class="btn btn-squared btn-danger margin-bottom-5 margin-right-10"><?php echo $word['app_shop_gosettlement'];?></a>
                                                    </li>
                                                    <li class="shop_cart_price">
                                                        <span class="font-size-18 topcart-total"></span>
                                                    </li>
                                                </ul>
                                        </div>
                                        </li>
                                                <?php if($ui[head_top_search_ok]){ ?>
                                                <div class="vertical-align-middle shopcart-btn login-btn search-index">
                                                    <form method="get" action="<?php echo $c['index_url'];?>search/search.php?lang=<?php echo $_M[lang];?>">
                                                        <input type="hidden" name="lang" value="<?php echo $_M[lang];?>">
                                                        <div class="input-search">
                                                            <button type="submit" class="input-search-btn"><i class="icon wb-search" aria-hidden="true"></i></button>
                                                            <input type="text" class="search" name="searchword" value="" placeholder="<?php echo $ui['search'];?>" data-fv-notempty="true" data-fv-message="不能为空">
                                                        </div>
                                                    </form>
                                                </div>
                                            <?php } ?>
                                </ul>
                            <?php }else{ ?>
                                <!-- 未登录状态开始 -->
                                 <ul class=" menu--dustu met-head-user met-nav-login" m-id="member" m-type="member">
                                    <li class="arrow-left menu__item">
                                        <a class="mp-back display menu__item-name" href="#" title="<?php echo $ui['nav_back'];?>">
                                            <i class="icon fa-angle-left nav-angle-left" aria-hidden="true"></i>
                                            <span><?php echo $ui['nav_back'];?></span>
                                        </a>
                                    </li>
                                    <li class="arrow-right menu__item">
                                        <a href="<?php echo $_M[url][site];?>member/login.php?lang=<?php echo $_M[lang];?>" class="btn-outline display menu__item-name">
                                            <span><?php echo $word['login'];?></span>
                                        </a>
                                    </li>
                                    <li class="arrow-right menu__item">
                                        <a href="<?php echo $_M[url][site];?>member/register_include.php?lang=<?php echo $_M[lang];?>" class="btn-success display menu__item-name">
                                            <span><?php echo $word['register'];?></span>
                                        </a>
                                    </li>
                                </ul>
                                <!-- 未登录状态开始 -->
                            <?php } ?>
                        <?php } ?>
                        </div>
                    </li>
                <?php } ?>
            <!-- 登录 注册结束 -->
            </ul>
            <!--一级导航结束 -->
            <div class="right-head-wx">
               <ul>
                        <?php if($ui['footinfo_wx_ok']){ ?>
                        <li>
                            <?php $img=strstr($ui['footinfo_wx'],"http"); ?>
                            <a  class="icon style2 fa-weixin" id="met-weixin" data-plugin="webuiPopover" data-trigger="hover" data-animation="pop" data-placement='top' data-width='155' data-padding='0' data-content="<div class='text-xs-center'>    <?php if($img){ ?><img src='<?php echo $ui['footinfo_wx'];?>' alt='<?php echo $c['met_webname'];?>' width='150' height='150' id='met-weixin-img'><?php } ?></div>"></a>
                        </li>
                    <?php } ?>
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
                    <?php if($ui['tel_txt']){ ?>
                    <div class="tel"><?php echo $ui['tel_txt'];?></div>
                <?php } ?>
            </div>
        </div>
        
    </nav>
    <!-- 侧边导航结束 -->
    <!-- 头部功能集合开始 -->
    <div id="menu" class="$uicss-top-head $uicss-dustu <?php echo $ny_mark_ok;?>"  m-id='<?php echo $ui['mid'];?>' m-type='head_nav'>
        <div class="container">
            <!-- LOGO开始-->
            <div class="head-logo" m-id='<?php echo $ui['mid'];?>'>
                <a href="<?php echo $c['index_url'];?>" class="met-logo vertical-align navbar-logo" title="<?php echo $c['met_webname'];?>">
                    <div class="vertical-align-middle">
                        <img src="<?php echo $c['met_logo'];?>" alt="<?php echo $c['met_webname'];?>" title="<?php echo $c['met_webname'];?>">
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
                        <?php if($data['classnow']<>10001){ ?>
                        <?php
    $type=strtolower(trim('son'));
    $cid=$data['releclass1'];
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
            $m['class']="active";
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
                            <li role='presentation' class='menu__item <?php echo $m['class'];?>'>
                                <a class='menu__item-name' href='<?php echo $m['url'];?>'><?php echo $m['name'];?></a>
                            </li>
                        <?php endforeach;?>
                    <?php } ?>
                </ul>
            </div>
            <!-- 首页引导菜单结束-->
        </div>
    </div>
    <!-- 头部功能集合结束 -->
    <!-- 主要内容开始 -->
    <div id="fullpage" class="$uicss-fullpage">
