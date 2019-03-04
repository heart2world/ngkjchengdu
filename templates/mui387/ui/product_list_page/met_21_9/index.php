<?php defined('IN_MET') or exit('No permission'); ?>
<if value="$ui['iframe']">
    <div m-id='{$ui.mid}'>
        <iframe name="homeIframe" width="100%" src="{$ui.iframe_link}" ></iframe>
    </div>
<else/>
<div class="$uicss met-product animsition lh-other-box met-index-body" m-id='{$ui.mid}'>
    <div class="container lh-other-container">
    	<tag action='product.list' num="$c['met_product_list']"></tag>
		<if value="$sub">
        <h2 class="lh-news-h cuisine">
            <b>{$data.name}</b>
            <span>{$data.namemark}</span>
        </h2>
        <h3 class="lh-news-h3">{$data.description}</h3>
		<ul class="lh-cusine-item clearfix lh-flex <if value="$ui['column_xs'] eq 1">
			block-xs-100
			<else/>
			blocks-xs-{$ui.column_xs}
			</if>
		 	blocks-md-{$ui.column_md} blocks-lg-{$ui.column_lg} blocks-xxl-{$ui.column_xxl}  met-pager-ajax imagesize met-product-list met-grid" id="met-grid" data-scale='{$c.met_productimg_y}x{$c.met_productimg_x}'>
            <if value="$c[met_product_page] eq 1 && $data['sub']">
                <tag action="category" type="son" cid="$data['classnow']"> 
                    <li class='p-r-15  item' >
                        <div class="card ">
                            <figure class="overlay overlay-hover">
                                <img class="overlay-figure" src="{$m.columnimg|thumb:$c['met_productimg_x'],$c['met_productimg_y']}" alt="{$m.name}">
                                <figcaption class="overlay-panel lanmu overlay-background overlay-fade overlay-icon" met-imgmask>
                                  <a class="fa fa-search" href="{$m.url}" {m.urlnew}></a>
                                </figcaption>
                            </figure>
                            <h4 class="card-title p-y-20 p-l-0 font-szie-16 ">
                                <a href="{$m.url}" title="{$m.title}" class="block text-truncate" {m.urlnew}>{$m.name}</a>
                                <p class="keyword">{$m.keywords}</p>
                            </h4>
                        </div>
                    </li>
                </tag>
            <else/>
    			<list data="$result" name="$l">
                    <li data-animated="fadeInUp" class="wow animated fadeInUp animated " data-plugin="appear" data-animate="slide-bottom" data-repeat="false">
                        <a href="{$l.url}" title="{$l.title}" {$g.urlnew}>
                            <div class="img">
                                <img src="{$l.imgurl|thumb:$c['met_productimg_x'],$c['met_productimg_y']}">
                            </div>
                            <h2>
                                <p>{$l.title}</p>
                                <span href="{$l.url}">
                                    <em>{$ui.more}</em>
                                    <i class="icon fa-long-arrow-right"></i>
                                </span>
                            </h2>
                            <if value="$ui['des_ok']">
                            <h3>
                                <?php $slh = strlen($l['description']) > $ui['long']?"...":" "; ?>
                                            {$l.description|met_substr:0,$ui['long']}{$slh}
                            </h3>
                            </if>
                            <if value="$v[price_str]">
                                <p class="prices">{$l.price_str}</p>
                            </if>
                        </a>
                    </li>
                </list>
            </if>
		</ul>
		<if value="!$c['met_product_page']">
			<div class='m-t-20 text-xs-center hidden-sm-down' m-type="nosysdata">
			    <pager/>
			</div>
			<div class="met_pager met-pager-ajax-link hidden-md-up" data-plugin="appear" data-animate="slide-bottom" data-repeat="false" m-type="nosysdata">
			    <button type="button" class="btn btn-primary btn-block btn-squared ladda-button" id="met-pager-btn" data-plugin="ladda" data-style="slide-left" data-url="" data-page="1">
			        <i class="icon wb-chevron-down m-r-5" aria-hidden="true"></i>
			        {$lang.page_ajax_next}
			    </button>
			</div>
		</if>
		<else/>
		<div class='h-100 text-xs-center font-size-20 vertical-align'>{$lang.nodata}</div>
		</if>
    </div>
 </div>
</if>
