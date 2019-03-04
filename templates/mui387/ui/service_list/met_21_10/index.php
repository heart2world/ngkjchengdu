<?php defined('IN_MET') or exit('No permission'); ?>
<?php
    if($_M['form']['pageset']){
        $pullpage_id = explode("<m",$ui['pullpage_id']);
        $pullpage_id = $pullpage_id[0];
        
    }else{
        $pullpage_id = $ui['pullpage_id'];
    }
?>
<div data-name="{$ui.pullpage_name}" data-id="{$pullpage_id}"  data-fun="$uicss_{$pullpage_id}" class="$uicss section met-index-product met-index-body met-index-newproduct <if value="$ui['bg_type']">bgcolor<else/>bgpic</if>" m-id='{$ui.mid}'>
    <div class="wrapper">
        <div class="">
            <div class="slideshow">
                <section class="strips">
                    <tag action="category" cid="$ui['id']" type="son" >
                        <?php
                            $counts = count($result);
                            $box_w = 100/$counts;
                            $box_left = $m['_index'] * $box_w;
                            $animate = $m['_index']%2 == 0?"slide-top":"slide-bottom";
                            $delay = $m['_index']*100;
                        ?>
                      <article data-modh="{$box_w}" data-index="{$m._index}" class="strips__strip" style="width:{$box_w}%;left:{$box_left}%">
                        <div class="strip__content invisible animation-delay-{$delay}" data-plugin="appear" data-animate="{$animate}" data-repeat="false" style="background:url({$m.columnimg}) no-repeat;background-size: cover;background-position: center;">
                            <h2 class="strip__title" data-name="Lorem" m-id='{$ui.mid}'>{$m.name}</h2>
                            <if value="$ui['box_type']">
                                <div class="strip__inner-text" data-hex="{$ui.box_txt_bg}|{$ui.box_txt_opt}">
                                    <h4>{$m.namemark}</h4>
                                    <?php $img=strstr($m['indeximg'],"upload"); ?>
                                    <if value="$img">
                                        <img src="{$m.indeximg}" alt="{$m.name}" style="max-width:100%;" />
                                    </if>
                                    <p>
                                      {$m.description}
                                    </p>
                                    <div>
                                        <a <if value="$ui['iflink']">href="{$m.url}"<else/>href="{$ui.link}"</if>  class="set_1_btn Vbtn-1" title="{$m.namemark}" {$m.urlnew}>
                                            <svg>
                                                <rect x="0" y="0" fill="none" width="100%" height="100%"></rect>
                                            </svg>
                                            <span>{$ui.box_more_txt}</span>
                                        </a>
                                    </div>
                                </div>
                            <else/>
                                <div class="met-editor strip__inner-text">
                                    {$m.content}
                                </div>
                            </if>
                        </div>
                        <i class="fa fa-close strip__close"></i>
                      </article>
                    </tag>
                </section>
            </div>
        </div>
    </div>
</div>