<?php defined('IN_MET') or exit('No permission'); ?>
<main class="$uicss  met-showproduct pagetype1" m-id='{$ui.mid}'>
    <div class="met-showproduct-head page-content block-bg">
        <div class="top_img">
            <h2 class="h2_center"></h2>
        <div class="container">
            <div class="row toprow">
                <div class="leftcenter col-lg-5">
                            <div class="product-intro">
                            <h1 class='m-t-0 font-size-24'>{$data.title}</h1>
                            <if value="$data['para']">
                            <ul class="product-para paralist blocks-100 blocks-sm-2 blocks-md-3 blocks-lg-2 m-x-0 p-y-5 <list data='$data[para]' name='$para'><if value='!$para[value] || !($s[_index] egt $ui[para_num])'>noboder</if></list>">
                                <list data="$data['para']" name="$para">
                                    <if value="$para[value] && $para[_index] egt $ui[para_num]">
                                        <li class="p-x-0 p-r-15">
                                            <span>{$para.name}：</span>
                                          {$para.value}
                                        </li>
                                    </if>
                                </list>
                            </ul> 
                            </if>
                             <!-- 产品详情页文件 我的新添加的 -->
                            <if value="$data['para_url']">
                            <div class='m-t-10'>
                                <list data="$data['para_url']" name="$s" num='100'>
                                <if value="$s['value']">
                                <a href="{$s.value}" class="btn btn-danger m-r-20" target="_blank">{$s.name}</a>
                                </if>
                                </list>
                            </div>
                            </if>
                        </div>
                        <div class="product-intro_2">
                          <if value="$data['addtime']">
                               <p class="addtime">{$word.AddDate}：{$data.addtime}</p>
                            </if>
                            <if value="$data['description']">
                            <p class='description'>{$data.description}</p>
                            </if> 
                        </div>
               
                </div>
                <div class="rightcenter col-lg-7">
                    <list data="$data['displayimgs']" name="$s"></list>
                        <div data-padh="{$ui.img_y}" class='met-showproduct-list fngallery cover text-xs-center<if value="$s[_index] gt 1">slick-dotted</if>' id='met-imgs-slick' m-id='noset' m-type="displayimgs">
                            <!--fngallery：启用lightGallery插件的类名-->
                            <list data="$data['displayimgs']" name="$s">
                            <div class='slick-slide'>
                                <a href='{$s.img}' data-size='{$s.x}x{$s.y}' data-med='{$s.img}' data-med-size='{$s.x}x{$s.y}' class='lg-item-box' data-src='{$s.img}' data-exthumbimage="{$s.img|thumb:$ui['img_x'],$ui['img_y']}" data-sub-html='{$s.title}'>
                                    <!--类名lg-item-box之前为initPhotoSwipeFromDOM插件所用参数；之后为lightGallery插件所用参数，lg-item-box：lightGallery插件对应的类名-->
                                    <img
                                    <if value="$s['_index'] gt 0">data-lazy<else/>src</if>
                                    ="{$s.img|thumb:$c['met_productdetail_x'],$c['met_productdetail_y']}" class='img-fluid' alt='{$s.title}' />
                                </a>
                            </div>
                            </list>
                        </div>
                </div>
            </div>
        </div>
    </div>
</div>
    <div class="met-showproduct-body page-content">
        <div class="container">
            <div class="row">
                <if value="$ui[product_sidebar_piclist_ok]">
<!-- 内容选项卡 -->
                <div class="col-lg-9 <if value='$ui[compos] eq 1'>pull-lg-right</if>">
                    <else/>
                <div class="col-lg-12">
                    </if>    
                    <div class="row">
                        <div class="panel panel-body m-b-0 product-detail" boxmh-mh >
                            <!-- 这个是导航ui -->
                            <ul class="nav nav-tabs nav-tabs-line m-b-20 met-showproduct-navtabs">
                                <list data="$data['contents']" name="$s">
                                <li class="nav-item">
                                    <a
                                    class='nav-link mynav-link
                                    <if value="$s['_first']">
                                    active
                                    </if>
                                    ' data-toggle="tab" href="#product-content{$s._index}" data-get="product-details">{$s.title}</a>
                                </li>
                                </list>
                            </ul>
                            <!-- 这个是内容 -->
                            <article class="tab-content">
                                <list data="$data['contents']" name="$s">
                                <section class="tab-pane met-editor clearfix animation-fade
                                    <if value="$s['_first']">
                                    active
                                    </if>
                                    " id="product-content{$s._index}">
                                    {$s.content}
                                </section>
                                </list>
                                <if value="$ui['tag_ok']">
                                    <div class="tag">
                                        <span>{$data.tagname}</span>
                                        <list data="$data[taglist]" name="$tag" num="$ui[tag_num]">
                                            <a href="{$tag.url}" title="{$tag.name}">{$tag.name}</a>
                                        </list>
                                    </div>
                                </if>
                            </article>
                        </div>
                    </div>
                </div>
<!-- 内容选项卡 -->

                <if value="$ui[product_sidebar_piclist_ok]"> 
<!-- 产品侧栏 -->
                <div class="col-lg-3 <if value='$ui[compos] eq 0'>pull-lg-right</if>">
                    <div class="row celan">
                        <aside class="panel panel-body m-b-0 product-hot met-sidebar <if value='$ui[compos] eq 1'>leftsidebar<else/>rightsidebar</if>" boxmh-h>
                            <div class='sidebar-piclist'>
                                <h3 class='m-0 font-size-16 font-weight-300'>{$ui.product_sidebar_piclist_title}</h3>
                                <ul class='blocks-2 blocks-md-3 blocks-lg-100 m-t-20 text-xs-center imagesize sidebar-piclist-ul' data-scale='{$v.img_y}x{$v.img_x}'>
                                    <tag action='list' cid="$data['class1']" num="$ui['product_sidebar_piclist_num']" type="$ui['product_sidebar_piclist_type']">
                                    <li class='masonry-child'>
                                        <a href='{$v.url}' title='{$v.title}' class='block m-b-0' target='_blank'>
                                            <img src="{$v.imgurl|thumb:$v['img_x'],$v['img_y']}" class='cover-image' alt='{$v.title}' height='100'></a>
                                        <h4 class='m-t-10 m-b-0 font-size-14 text-md-center'>
                                            <a href='{$v.url}' title='{$v.title}' target='_blank'>{$v.title}</a>
                                        </h4>
                                    </li>
                                    </tag>
                                </ul>
                            </div>
                        </aside>
                    </div>
                </div>
<!-- 产品侧栏 -->
            </if>

            </div>
        </div>
    </div>
</main>

