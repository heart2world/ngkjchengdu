<?php defined('IN_MET') or exit('No permission'); ?>
<main class="$uicss page-content" m-id='{$ui.mid}'>
    <div class="container">
        <div class="row">
        <if value="$data[index_num] eq 213 && $data[sub]">
            <ul class="team_list blocks-100 blocks-sm-2 blocks-md-3 blocks-xlg-4  clearfix">
                <tag action="category" type="son" cid="$data[id]">
                    <li class="team_item">
                        <a href="{$m.url}" title="{$m.name}" {$m.urlnew}>
                            <div class="team_img">
                                <img src="{$m.columnimg}" alt="{$m.name}"/>
                            </div>
                        <div class="wrap" met-imgmask>
                            <div>
                                <span class="h"></span>
                                <span class="v"></span>
                            </div>
                        </div>
                        </a>
                    <div class="team_info">
                        <p class="title">{$m.name}</p>
                        <p class="keywords">{$m.keywords}</p>
                        <p class="desc">{$m.description}</p>
                    </div>
                    </li>
                </tag>
            </ul>
        <elseif value="$data[index_num] eq 214"/>
            <div class="show_item">
                <div class="show_item_left col-sm-6 invisible" data-plugin="appear" data-animate="slide-right50" data-repeat="false">
                    <div class="show_item_left_bor">
                        <img src="{$data.columnimg}" class="invisible" alt="{$data.name}" data-plugin="appear" data-animate="slide-left50" data-repeat="false">
                    </div>
                </div>
                <div class="show_item_right col-sm-6 invisible"  data-plugin="appear" data-animate="slide-bottom50" data-repeat="false">
                    {$data.content}
                </div>
            </div>
            <div class="time_axis">
                <div class="time_axis_top">{$ui.time_top}</div>
                <div class="ad-zt">
                    <tag action="category" cid="$ui['id_zt']" type="son">
<?php
    $contxt = $m['_index']%2?"pull-right":"pull-left ";
    $conimg = $m['_index']%2?"pull-left":"pull-right";
    $animate = $m['_index']%2?"slide-left50":"slide-right50";
    $index_num = $m['_index']%2?"ad-zt-img-num-left":"ad-zt-img-num-right";
    $num = $m['_index']>9?$m['_index']+1:"0".($m['_index']+1);
?>
                        <div class="col-sm-12 col-xs-12" data-plugin="appear" data-animate="{$animate}" data-repeat="false">
                            <div class="col-sm-6 col-xs-12 {$contxt} ad-zt-txt">
                                <h3 class="ad-zt-txt-02">{$m.name}</h3>
                            </div>
                            <div class="time-btns">
                                <div class="time-btn">
                                    <i class="icon fa-{$ui.time_icon}" aria-hidden="true" style="font-size: 25px;"></i>
                                </div>
                            </div>
                            <div class="col-sm-6 col-xs-12 {$conimg} ad-zt-img {$ui.img_por}">
                                <div>
                                    <h3>{$m.namemark}</h3>
                                    <?php
                                        $str = strpos($m[columnimg],"public/");
                                        $strok = $str?false:true;
                                    ?>
                                    <if value="$strok">
                                        <img src="{$m.columnimg}" alt="{$m.name}" class="img-responsive" style="max-width:100%;">
                                    </if>
                                    <p>{$m.description}</p>
                                </div>
                            </div>
                        </div>
                    </tag>
                </div>
                <div class="time_axis_top">{$ui.time_bottom}</div>
            </div>
        <else/>
            <article class="met-show-body panel panel-body m-b-0" boxmh-mh>
                <div class="container">
                    <section class="met-editor clearfix">
                        <if value="$data['content']">
                            {$data.content}
                        <else/>
                            <div class='h-100 text-xs-center font-size-20 vertical-align'>{$lang.nodata}</div>
                        </if>
                    </section>
                </div>
            </article>
        </if>
        </div>
    </div>
</main>