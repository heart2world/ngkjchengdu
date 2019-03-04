<?php defined('IN_MET') or exit('No permission'); ?>
<?php $met_page = "index";?>
<?php
$metinfover_v2=$c["metinfover"]=="v2"?true:false;
$met_file_version=str_replace(".","",$c["metcms_v"]).$c["met_patch"];
$lang_json_file_ok=file_exists(PATH_WEB."cache/lang_json_".$_M["lang"].".js");
if(!$lang_json_file_ok){
    echo "<meta http-equiv='refresh' content='0'/>";
}
$html_hidden=$lang_json_file_ok?"":"hidden";
if(!$data["module"] || $data["module"]==10){
    $nofollow=1;
}
$user_name=$_M["user"]?$_M["user"]["username"]:"";
?>
<!DOCTYPE HTML>
<html class="<?php echo $html_class;?>" <?php echo $html_hidden;?>>
<head>
<meta charset="utf-8">
<?php if($nofollow){ ?>
<meta name="robots" content="noindex,nofllow" />
<?php } ?>
<meta name="renderer" content="webkit">
<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
<meta name="viewport" content="width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=0,minimal-ui">
<meta name="format-detection" content="telephone=no">
<title><?php echo $data['page_title'];?></title>
<meta name="description" content="<?php echo $data['page_description'];?>">
<meta name="keywords" content="<?php echo $data['page_keywords'];?>">
<meta name="generator" content="MetInfo <?php echo $c['metcms_v'];?>" data-variable="<?php echo $c['met_weburl'];?>|<?php echo $data['lang'];?>|<?php echo $data['synchronous'];?>|<?php echo $c['met_skin_user'];?>|<?php echo $data['module'];?>|<?php echo $data['classnow'];?>|<?php echo $data['id'];?>" data-user_name="<?php echo $user_name;?>">
<link href="<?php echo $c['met_weburl'];?>favicon.ico" rel="shortcut icon" type="image/x-icon">
<?php
if($lang_json_file_ok){
    $basic_css_name=$metinfover_v2?"":"_web";
    $isLteIe9=strpos($_SERVER["HTTP_USER_AGENT"],"MSIE 9")!==false || strpos($_SERVER["HTTP_USER_AGENT"],"MSIE 8")!==false;
    if($isLteIe9){
?>
<link href="<?php echo $url['site'];?>app/system/include/static2/css/bootstrap.min.css" rel="stylesheet" type="text/css">
<link href="<?php echo $url['site'];?>app/system/include/static2/css/bootstrap-extend.min.css" rel="stylesheet" type="text/css">
<link href="<?php echo $url['site'];?>app/system/include/static2/assets/css/site.min.css" rel="stylesheet" type="text/css">
<link href="<?php echo $url['site'];?>public/ui/v2/static/css/basic<?php echo $basic_css_name;?>-lteie9-1.css?<?php echo $met_file_version;?>" rel="stylesheet" type="text/css">
<?php }else{ ?>
<link rel="stylesheet" type="text/css" href="<?php echo $url['site'];?>public/ui/v2/static/css/basic<?php echo $basic_css_name;?>.css?<?php echo $met_file_version;?>">
<?php
    }
    if($metinfover_v2){
        if(file_exists(PATH_TEM."cache/common.css")){
            $common_css_time = filemtime(PATH_TEM."cache/common.css");
?>
<link rel="stylesheet" type="text/css" href="<?php echo $c['met_weburl'];?>templates/<?php echo $c['met_skin_user'];?>/cache/common.css?<?php echo $common_css_time;?>">
<?php
        }
        if($met_page){
            if($met_page == 404) $met_page = "show";
            $page_css_time = filemtime(PATH_TEM."cache/".$met_page."_".$_M["lang"].".css");
?>
<link rel="stylesheet" type="text/css" href="<?php echo $c['met_weburl'];?>templates/<?php echo $c['met_skin_user'];?>/cache/<?php echo $met_page;?>_<?php echo $_M["lang"];?>.css?<?php echo $page_css_time;?>">
<?php
        }
    }
    if(is_mobile() && $c["met_headstat_mobile"]){
?>
<?php echo $c['met_headstat_mobile'];?>

<?php }else if(!is_mobile() && $c["met_headstat"]){?>
<?php echo $c['met_headstat'];?>

<?php
    }
    if($_M["html_plugin"]["head_script"]){
?>
<?php echo $_M["html_plugin"]["head_script"];?>

<?php } ?>
<style>
body{
<?php if($g["bodybgimg"]){ ?>
    background-image: url(<?php echo $g['bodybgimg'];?>) !important;background-position: center;background-repeat: no-repeat;
<?php } ?>
    background-color:<?php echo $g['bodybgcolor'];?> !important;font-family:<?php echo $g['met_font'];?> !important;}
</style>
<!--[if lte IE 9]>
<script src="<?php echo $_M["url"]["site"];?>public/ui/v2/static/js/lteie9.js"></script>
<![endif]-->
</head>
<!--[if lte IE 8]>
<div class="text-xs-center m-b-0 bg-blue-grey-100 alert">
    <button type="button" class="close" aria-label="Close" data-dismiss="alert">
        <span aria-hidden="true">×</span>
    </button>
    <?php echo $word['browserupdatetips'];?>
</div>
<![endif]-->
<body>
<?php } ?>
        <?php
            $id = 2;
            $style = "met_21_14";
            if(!isset($ui_compile)){
                load::sys_class('view/ui_compile');
                $ui_compile = new ui_compile();
            }
            $ui = $ui_compile->list_local_config($id);
            $ui['has'] =$ui_compile->list_page_config($met_page);
            ?>
<?php
    $fullpage_ok = $data['classnow']== 10001?"ok":" ";
    $ny_mark_ok = $data['classnow'] != 10001 && $ui['ny_mark_bg']?"ny-mark":" ";
?>
    <?php if($ui['head_loader_ok']){ ?>
    <div class="head_nav_met_21_14-loader-box     <?php if($ui[head_loader_bg_type]){ ?>loader-bgcolor<?php }else{ ?>loader-bgpic<?php } ?>"> 
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
    <div class="head_nav_met_21_14-btn-top met-scroll-top <?php echo $fullpage_ok;?>" m-id='<?php echo $ui['mid'];?>' hidden>
        <button class="active" number="1" m-type="nocontent">
            <i class="icon fa-play" aria-hidden="true"></i>
        </button>
    </div>
<?php } ?>
<div class="mp-pusher head_nav_met_21_14-pusher" id="mp-pusher" data-fullpage="<?php echo $fullpage_ok;?>" data-mask="<?php echo $ui['Mask'];?>" data-scroll="<?php echo $ui['scroll_type'];?>">
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
    <nav id="mp-menu" class="mp-menu head_nav_met_21_14-menu head_nav_met_21_14-dustu">
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
    <div id="menu" class="head_nav_met_21_14-top-head head_nav_met_21_14-dustu <?php echo $ny_mark_ok;?>"  m-id='<?php echo $ui['mid'];?>' m-type='head_nav'>
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
                <svg class="ham ham6 head_nav_met_21_14-ham"  id="trigger" class="menu-trigger" viewBox="0 0 100 100" width="56">
                  <path class="line top" d="m 30,33 h 40 c 13.100415,0 14.380204,31.80258 6.899646,33.421777 -24.612039,5.327373 9.016154,-52.337577 -12.75751,-30.563913 l -28.284272,28.284272" />
                  <path class="line middle" d="m 70,50 c 0,0 -32.213436,0 -40,0 -7.786564,0 -6.428571,-4.640244 -6.428571,-8.571429 0,-5.895471 6.073743,-11.783399 12.286435,-5.570707 6.212692,6.212692 28.284272,28.284272 28.284272,28.284272" />
                  <path class="line bottom" d="m 69.575405,67.073826 h -40 c -13.100415,0 -14.380204,-31.80258 -6.899646,-33.421777 24.612039,-5.327373 -9.016154,52.337577 12.75751,30.563913 l 28.284272,-28.284272" />
                </svg>
            </a>
            <!-- 菜单按键结束-->
            <!-- 首页引导菜单开始-->
            <div class="pull-right top_ydhead nav-tabs-horizontal nav-tabs-inverse nav-tabs-animate head_nav_met_21_14-nav-tabs">
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
    <div id="fullpage" class="head_nav_met_21_14-fullpage">


        <?php
            $id = 35;
            $style = "met_21_7";
            if(!isset($ui_compile)){
                load::sys_class('view/ui_compile');
                $ui_compile = new ui_compile();
            }
            $ui = $ui_compile->list_local_config($id);
            $ui['has'] =$ui_compile->list_page_config($met_page);
            ?>
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
<div  data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>" data-fun="banner_met_21_7_<?php echo $pullpage_id;?>" data-funname="banner" class="section banner_met_21_7     <?php if($data['classnow']<>10001){ ?>banner-ny-h<?php } ?>"
    m-id='<?php echo $ui['mid'];?>' m-type="banner" data-hash="<?php echo $ui['hash'];?>" data-title="<?php echo $ui['hash'];?>">
        <?php if($ui['video'] && $data['classnow']==10001){ ?>
        <div class="banner_met_ met-video" data-Method="banner_met_21_7_<?php echo $pullpage_id;?>">
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
        <div class="banner_met_21_7-ny vertical-align text-xs-center" m-id='<?php echo $ui['mid'];?>' m-type='banner'>
            <h1 class="vertical-align-middle"><?php echo $m[name];?></h1>
        </div>
    <?php endforeach;?>
<?php } ?>
<?php } ?>


        <?php
            $id = 4;
            $style = "met_21_12";
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
?>
<div data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>" data-funname="about" data-fun="about_list_met_21_12_<?php echo $pullpage_id;?>" class="about_list_met_21_12 abouts section met-index-body     <?php if($ui['bg_type']){ ?>bgcolor<?php }else{ ?>bgpic<?php } ?>" m-id='<?php echo $ui['mid'];?>'>
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
                                    <div class="about_num_txt_num" data-parameter="<?php echo $ui['num_box1'];?>" data-Method="about_list_met_21_12_<?php echo $pullpage_id;?>">000</div>
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
                                    <div class="about_num_txt_num" data-parameter="<?php echo $ui['num_box2'];?>" data-Method="about_list_met_21_12_<?php echo $pullpage_id;?>">000</div>
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
                                    <div class="about_num_txt_num" data-parameter="<?php echo $ui['num_box3'];?>" data-Method="about_list_met_21_12_<?php echo $pullpage_id;?>">000</div>
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
                                    <div class="about_num_txt_num" data-parameter="<?php echo $ui['num_box4'];?>" data-Method="about_list_met_21_12_<?php echo $pullpage_id;?>">000</div>
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

        <?php
            $id = 3;
            $style = "met_21_10";
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
?>
<div data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>"  data-fun="service_list_met_21_10_<?php echo $pullpage_id;?>" class="service_list_met_21_10 section met-index-product met-index-body met-index-newproduct     <?php if($ui['bg_type']){ ?>bgcolor<?php }else{ ?>bgpic<?php } ?>" m-id='<?php echo $ui['mid'];?>'>
    <div class="wrapper">
        <div class="">
            <div class="slideshow">
                <section class="strips">
                    <?php
    $type=strtolower(trim('son'));
    $cid=$ui['id'];
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
                            $counts = count($result);
                            $box_w = 100/$counts;
                            $box_left = $m['_index'] * $box_w;
                            $animate = $m['_index']%2 == 0?"slide-top":"slide-bottom";
                            $delay = $m['_index']*100;
                        ?>
                      <article data-modh="<?php echo $box_w;?>" data-index="<?php echo $m['_index'];?>" class="strips__strip" style="width:<?php echo $box_w;?>%;left:<?php echo $box_left;?>%">
                        <div class="strip__content invisible animation-delay-<?php echo $delay;?>" data-plugin="appear" data-animate="<?php echo $animate;?>" data-repeat="false" style="background:url(<?php echo $m['columnimg'];?>) no-repeat;background-size: cover;background-position: center;">
                            <h2 class="strip__title" data-name="Lorem" m-id='<?php echo $ui['mid'];?>'><?php echo $m['name'];?></h2>
                                <?php if($ui['box_type']){ ?>
                                <div class="strip__inner-text" data-hex="<?php echo $ui['box_txt_bg'];?>|<?php echo $ui['box_txt_opt'];?>">
                                    <h4><?php echo $m['namemark'];?></h4>
                                    <?php $img=strstr($m['indeximg'],"upload"); ?>
                                        <?php if($img){ ?>
                                        <img src="<?php echo $m['indeximg'];?>" alt="<?php echo $m['name'];?>" style="max-width:100%;" />
                                    <?php } ?>
                                    <p>
                                      <?php echo $m['description'];?>
                                    </p>
                                    <div>
                                        <a     <?php if($ui['iflink']){ ?>href="<?php echo $m['url'];?>"<?php }else{ ?>href="<?php echo $ui['link'];?>"<?php } ?>  class="set_1_btn Vbtn-1" title="<?php echo $m['namemark'];?>" <?php echo $m['urlnew'];?>>
                                            <svg>
                                                <rect x="0" y="0" fill="none" width="100%" height="100%"></rect>
                                            </svg>
                                            <span><?php echo $ui['box_more_txt'];?></span>
                                        </a>
                                    </div>
                                </div>
                            <?php }else{ ?>
                                <div class="met-editor strip__inner-text">
                                    <?php echo $m['content'];?>
                                </div>
                            <?php } ?>
                        </div>
                        <i class="fa fa-close strip__close"></i>
                      </article>
                    <?php endforeach;?>
                </section>
            </div>
        </div>
    </div>
</div>

        <?php
            $id = 6;
            $style = "met_21_9";
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
?>
<div data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>"  data-fun="service_list_met_21_9_<?php echo $pullpage_id;?>" class="service_list_met_21_9 section met-index-product met-index-body met-index-newproduct     <?php if($ui['bg_type']){ ?>bgcolor<?php }else{ ?>bgpic<?php } ?>     <?php if($ui['heightif']){ ?>heightbgd<?php }else{ ?>heightgd<?php } ?>" m-id='<?php echo $ui['mid'];?>'>
    <div class="wrapper">
        <div class="container">
            <div class="slideshow">
                <div class="slideshow-left ">
                    <?php
    $type=strtolower(trim('son'));
    $cid=$ui['id'];
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
                            <?php if($m[_index]<1){ ?>
                            <div class="Lslide" style="background:<?php echo $m['namemark'];?>">
                                <div class="Lslide-content">
                                    <h2 class="invisible" data-plugin="appear" data-animate="slide-top" data-repeat="false"><?php echo $m['name'];?></h2>
                                    <p  class="invisible" data-plugin="appear" data-animate="slide-top" data-repeat="false"><?php echo $m['description'];?></p>
                                    <div class="button invisible" data-plugin="appear" data-animate="slide-bottom" data-repeat="false">
                                        <a     <?php if($ui['iflink']){ ?>href="<?php echo $m['url'];?>"<?php }else{ ?>href="<?php echo $ui['link'];?>"<?php } ?> title="<?php echo $m['name'];?>" <?php echo $m['urlnew'];?>     <?php if($m['nofollow']){ ?>rel="nofollow"<?php } ?>>
                                            <p><?php echo $ui['more'];?></p>
                                            <i class="fa fa-chevron-right" aria-hidden="true"></i>
                                        </a>
                                    </div>
                                </div>
                            </div>
                        <?php }else{ ?>
                            <div class="Lslide" style="background:<?php echo $m['namemark'];?>">
                                <div class="Lslide-content">
                                    <h2 class=""><?php echo $m['name'];?></h2>
                                    <p class=""><?php echo $m['description'];?></p>
                                    <div class="button ">
                                        <a     <?php if($ui['iflink']){ ?>href="<?php echo $m['url'];?>"<?php }else{ ?>href="<?php echo $ui['link'];?>"<?php } ?> title="<?php echo $m['name'];?>" <?php echo $m['urlnew'];?>     <?php if($m['nofollow']){ ?>rel="nofollow"<?php } ?>>
                                            <p><?php echo $ui['more'];?></p>
                                            <i class="fa fa-chevron-right" aria-hidden="true"></i>
                                        </a>
                                    </div>
                                </div>
                            </div>
                        <?php } ?>
                    <?php endforeach;?>
                </div>
            <div class="slideshow-right " >
                        <?php
            $sub = count($result);
            $num = 30;
            if(!is_array($result)){
                $result = explode('|',$result);
            }
            foreach ($result as $index => $val) {
                if($index >= $num){
                    break;
                }
                if($sub <=0){
                    continue;
                }
                if(is_array($val)){
                    $val['_index'] = $index;
                    $val['_first'] = $index == 0 ? true : false;
                    $val['_last']  = $index == (count($result)-1) ? true : false;
                    $val['sub']    = $sub;
                }

                $l = $val;
            ?>
                    <div class="Rslide" style="background:url(<?php echo $l['columnimg'];?>) no-repeat;background-size: cover;background-position: center;">
                    </div>
                <?php }?>
            </div>
            <div class="control">
                <div class="oncontrol control-top invisible" data-plugin="appear" data-animate="slide-left" data-repeat="false">
                    <i class="fa fa-circle-o" aria-hidden="true"></i>
                </div>
                <div class="oncontrol control-bottom invisible" data-plugin="appear" data-animate="slide-right" data-repeat="false">
                    <i class="fa fa-arrow-down" aria-hidden="true"></i>
                </div>
            </div>
            </div>
        </div>
    </div>
</div>

        <?php
            $id = 9;
            $style = "met_21_26";
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
?>
<div data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>" data-fun="product_list_met_21_26_<?php echo $pullpage_id;?>" class="product_list_met_21_26 abouts section met-index-body     <?php if($ui['bg_type']){ ?>bgcolor<?php }else{ ?>bgpic<?php } ?>" m-id='<?php echo $ui['mid'];?>'>
    <div class="product_box">
        <div class="row">
            <div class="hero-slides">
                <div class="headers" >
                    <?php
    $type=strtolower(trim('current'));
    $cid=$ui['id'];
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
                        <div class="logos invisible" data-plugin="appear" data-animate="slide-left" data-repeat="false">
                                <?php if($ui['title']){ ?>
                                <h3>
                                    <a href="<?php echo $m['url'];?>" title="<?php echo $ui['title'];?>" <?php echo $m['urlnew'];?>>
                                        <?php echo $ui['title'];?>
                                    </a>
                                </h3>
                            <?php } ?>
                                <?php if($ui['desc']){ ?>
                                <p><?php echo $ui['desc'];?></p>
                            <?php } ?>
                        </div>
                        <a href="<?php echo $m['url'];?>" title="<?php echo $ui['title'];?>" class="invisible" data-plugin="appear" data-animate="slide-right" data-repeat="false" <?php echo $m['urlnew'];?> >
                            <div class="menu">
                                <span class="more"><?php echo $ui['box_more'];?></span>
                                <div class="hamburgers">
                                    <div class="slice"></div>
                                    <div class="slice"></div>
                                    <div class="slice"></div>
                                </div>
                            </div>
                        </a>
                    <?php endforeach;?>
                </div>
                <div class="swiper-container invisible" m-id='<?php echo $ui['mid'];?>' data-num="<?php echo $ui['list_nums'];?>" data-listnum="<?php echo $ui['list_num'];?>" data-plugin="appear" data-animate="slide-right" data-repeat="false">
                    <div class="swiper-wrapper">
                            <?php
    $cid=$ui['id'];

    $num = $ui['list_num'];
    $module = "";
    $type = $ui['list_type'];
    $order = 'no_order asc';
    $para = "";
    if(!$module){
        if(!$cid){
            $value = $m['classnow'];
        }else{
            $value = $cid;
        }
    }else{
        $value = $module;
    }

    $result = load::sys_class('label', 'new')->get('tag')->get_list($value, $num, $type, $order, $para);
    $sub = count($result);
    foreach($result as $index=>$v):
        $id = $v['id'];
        $v['sub'] = $sub;
        $v['_index']= $index;
        $v['_first']= $index==0 ? true:false;
        $v['_last']=$index==(count($result)-1)?true:false;
        $$v = $v;
?>
                                <?php   ?>
                                <div class="swiper-slide"     <?php if($ui['img_type']){ ?>style="background-image: url(<?php echo $v['imgurl'];?>);"<?php } ?>>
                                        <?php if(!$ui['img_type']){ ?>
                                        <img src="<?php echo thumb($v['imgurl'],$ui['img_w'],$ui['img_h']);?>" alt="<?php echo $v['title'];?>" style="max-width:100%;">
                                    <?php } ?>
                                    <?php  $indexe = $v['_index'] >= 9?$v['_index']+1:"0".($v['_index']+1); ?>
                                    <div class="number"><?php echo $indexe;?></div>
                                    <div class="body">
                                        <div class="location">
                                            <p><?php echo $v['description'];?></p>
                                                <?php if($v['price_str']){ ?>
                                                <p class="price"><?php echo $v['price_str'];?></p>
                                            <?php } ?>
                                        </div>
                                        <div class="headline"><?php echo $v['title'];?></div>
                                        <a href="<?php echo $v['url'];?>" title="<?php echo $v['title'];?>" <?php echo $g['urlnew'];?>>
                                            <span class="link"><?php echo $ui['more'];?></span>
                                        </a>
                                    </div>
                                </div>
                            <?php endforeach;?>
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

        <?php
            $id = 10;
            $style = "met_21_15";
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
?>
<div data-name="<?php echo $ui['pullpage_name'];?>" data-id="<?php echo $pullpage_id;?>" data-fun="news_list_met_21_15_<?php echo $pullpage_id;?>" class="news_list_met_21_15 abouts section met-index-body     <?php if($ui['bg_type']){ ?>bgcolor<?php }else{ ?>bgpic<?php } ?>" m-id='<?php echo $ui['mid'];?>'>
    <div class="">
        <div class="">
            <div class="hero-slides" m-id='<?php echo $ui['mid'];?>'>
                <div class="title_box">
                        <?php if($ui['title']){ ?>
                        <h3 class="invisible" data-plugin="appear" data-animate="slide-top" data-repeat="false">
                            <?php
    $type=strtolower(trim('current'));
    $cid=$ui['id'];
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
                                <a href="<?php echo $m['url'];?>" title="<?php echo $ui['title'];?>" <?php echo $m['urlnew'];?>><?php echo $ui['title'];?></a>
                            <?php endforeach;?>
                        </h3>
                    <?php } ?>
                        <?php if($ui['desc']){ ?>
                        <p class="invisible" data-plugin="appear" data-animate="slide-bottom" data-repeat="false"><?php echo $ui['desc'];?></p>
                    <?php } ?>
                </div>
                <div class="swiper-container invisible" data-num="<?php echo $ui['list_nums'];?>" data-listnum="<?php echo $ui['list_num'];?>" data-plugin="appear" data-animate="slide-bottom" data-repeat="false">
                    <div class="swiper-wrapper grid">
                        <?php
    $cid=$ui['id'];

    $num = $ui['list_num'];
    $module = "";
    $type = $ui['list_type'];
    $order = 'no_order asc';
    $para = "";
    if(!$module){
        if(!$cid){
            $value = $m['classnow'];
        }else{
            $value = $cid;
        }
    }else{
        $value = $module;
    }

    $result = load::sys_class('label', 'new')->get('tag')->get_list($value, $num, $type, $order, $para);
    $sub = count($result);
    foreach($result as $index=>$v):
        $id = $v['id'];
        $v['sub'] = $sub;
        $v['_index']= $index;
        $v['_first']= $index==0 ? true:false;
        $v['_last']=$index==(count($result)-1)?true:false;
        $$v = $v;
?>
                            <div class="swiper-slide">
                                <figure class="effect-sarah">
                                    <img src="<?php echo thumb($v['imgurl'],$ui['img_w'],$ui['img_h']);?>" alt="<?php echo $v['title'];?>"/>
                                    <figcaption>
                                        <h4><?php echo $v['title'];?></h4>
                                        <p><?php echo $v['description'];?></p>
                                        <a href="<?php echo $v['url'];?>" title="<?php echo $v['title'];?>" <?php echo $g['urlnew'];?>></a>
                                    </figcaption>
                                </figure>
                            </div>
                        <?php endforeach;?>
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