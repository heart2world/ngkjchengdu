<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    if($_M['form']['pageset']){
        $pullpage_id = explode("<m",$ui['pullpage_id']);
        $pullpage_id = $pullpage_id[0];
        
    }else{
        $pullpage_id = $ui['pullpage_id'];
    }
?>
<div data-name="{$ui.pullpage_name}" data-id="{$pullpage_id}"  data-fun="$uicss_{$pullpage_id}" class="$uicss section met-index-product met-index-body met-index-newproduct <if value="$ui['bg_type']">bgcolor<else/>bgpic</if> <if value="$ui['heightif']">heightbgd<else/>heightgd</if>" m-id='{$ui.mid}'>
    <div class="wrapper">
        <div class="container">
            <div class="slideshow">
                <div class="slideshow-left ">
                    <tag action="category" cid="$ui['id']" type="son" >
                        <if value="$m[_index] lt 1">
                            <div class="Lslide" style="background:{$m.namemark}">
                                <div class="Lslide-content">
                                    <h2 class="invisible" data-plugin="appear" data-animate="slide-top" data-repeat="false">{$m.name}</h2>
                                    <p  class="invisible" data-plugin="appear" data-animate="slide-top" data-repeat="false">{$m.description}</p>
                                    <div class="button invisible" data-plugin="appear" data-animate="slide-bottom" data-repeat="false">
                                        <a <if value="$ui['iflink']">href="{$m.url}"<else/>href="{$ui.link}"</if> title="{$m.name}" {$m.urlnew} <if value="$m['nofollow']">rel="nofollow"</if>>
                                            <p>{$ui.more}</p>
                                            <i class="fa fa-chevron-right" aria-hidden="true"></i>
                                        </a>
                                    </div>
                                </div>
                            </div>
                        <else/>
                            <div class="Lslide" style="background:{$m.namemark}">
                                <div class="Lslide-content">
                                    <h2 class="">{$m.name}</h2>
                                    <p class="">{$m.description}</p>
                                    <div class="button ">
                                        <a <if value="$ui['iflink']">href="{$m.url}"<else/>href="{$ui.link}"</if> title="{$m.name}" {$m.urlnew} <if value="$m['nofollow']">rel="nofollow"</if>>
                                            <p>{$ui.more}</p>
                                            <i class="fa fa-chevron-right" aria-hidden="true"></i>
                                        </a>
                                    </div>
                                </div>
                            </div>
                        </if>
                    </tag>
                </div>
            <div class="slideshow-right " >
                <list data="$result" name="$l">
                    <div class="Rslide" style="background:url({$l.columnimg}) no-repeat;background-size: cover;background-position: center;">
                    </div>
                </list>
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