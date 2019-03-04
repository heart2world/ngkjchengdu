<?php defined('IN_MET') or exit('No permission'); ?>
<div class="$uicss met-index-body lh-other-box <if value='$ui[bg_type]'>bgcolor<else/>bgpic</if>" m-id="{$ui.mid}">
	<div class="
		<if value='$ui[is_full] eq 1'>
			container-fiuled full
		<else/>
			container
		</if>
	 ">
		<div class="portfolio-masonry-wrapper lh-other-container" data-col="3" data-gutter="30">
			<if value="$ui['titleok']">
				<h2 class="lh-viewpoint-h2 lh-fonts" data-animated="fadeInUp"><span>{$data.name}</span></h2>
			</if>
			<h3 class="lh-viewpoint-h3" data-animated="fadeInUp">{$data.namemark}</h3>
			<h4 class="lh-viewpoint-h4" data-animated="fadeInUp">{$data.description}</h4>
			<tag action='img.list' num="$c[met_img_list]" cid="$data[classnow]">
			</tag>
			<ul id="portfolio-list" class="lh-viewpoint-item business clearfix fngallery portfolio-list no-space met-pager-ajax imagesize met-img-list <if value='$ui[mobile_img_num] eq 1'>
			blocks-100
			<else/>
			blocks-xs-{$ui.mobile_img_num}
			</if>
			blocks-md-{$ui.ipad_img_num} blocks-lg-{$ui.img_num} blocks-xxl-{$ui.lg_img_num}" data-scale='{$c.met_imgs_y}x{$c.met_imgs_x}' m-id='{$ui.mid}'>
				<tag action='img.list' num="$c['met_img_list']" cid="$data[classnow]">
					<li class="portfolio-item-wrap" data-plugin="appear" data-animate="slide-bottom" data-repeat="false">
						<a href="/a/gongsiyewu/shitangtuoguanchengbao/" class="lh-mobile-a"></a>
						<i></i>
						<div class="img">
							<a href="{$v.url}" title="{$v.title}">
								<img src="{$v.imgurl}" /></div>
							</a>
						<div class="text">
			            <div class="lh-business-a">
			            	<font class="iconfont icon-bus1"></font>
			              	<h2 class="lh-fonts">{$v.title}</h2>
			              	<h3 class="lh-viewpoint-res">{$v.description}</h3>
			              	<a href="{$v.url}" class="lh-more" title="{$v.title}">
			              		<span>{$ui.more}</span>
			              		<i class="icon fa-long-arrow-right"></i>
			              	</a>
			          	</div>
			          </div>
			        </li>
				</tag>
			</ul>
		</div>


		<!-- 按钮 -->
		<div class='m-t-20 text-xs-center hidden-sm-down' m-type="nosysdata">
            <pager/>
        </div>
        <div class="met_pager met-pager-ajax-link hidden-md-up" data-plugin="appear" data-animate="slide-bottom" data-repeat="false" m-type="nosysdata">
            <button type="button" class="btn btn-primary btn-block btn-squared ladda-button" id="met-pager-btn" data-plugin="ladda" data-style="slide-left" data-url="" data-page="1">
                <i class="icon wb-chevron-down m-r-5" aria-hidden="true"></i>
            </button>
        </div>
        <!-- /按钮 -->
        
	</div>
</div>