<?php defined('IN_MET') or exit('No permission'); ?>
<section class="$uicss page-content" m-id="{$ui.mid}">
	<div class="container-fiuled main-content">
		<if value="$ui['left_tag']">
			<div class="content-left">
				<div class="img-box">
					<img src="{$ui.left_img}" class="left-img" />
					<div class="txt-content">
						<if value="$ui['title']">
							<p class="title">{$ui.title}</p>
						</if>
						<if value="$ui['desc']">
							<span class="desc">{$ui.desc}</span>
						</if>
						<div class="all-content">{$ui.left_bottom_edit}</div>
					</div>
				</div>
			</div>
		</if>
		<div class="content-right <if value='$ui[left_tag] eq 0'>no-left</if>">
			<ul class="met-pager-ajax">
				<tag action='job.list' num="$c['met_job_list']" cid="$data[classnow]">
					<li>
						<p class="my-title">{$v.position}</p>
						<a class="my-btn" href="{$v.url}" alt="{$v.title}">查看职位<i class="icon fa-long-arrow-right" aria-hidden="true" style="font-size: 15px;"></i></a>
					</li>
				</tag>
			</ul>
			<if value="!$c['met_img_page'] || !$data['sub']">
	            <div class='$uicss-pager text-xs-center hidden-sm-down' m-type="nosysdata">
	                <pager/>
	            </div>
	            <div class="met_pager met-pager-ajax-link hidden-md-up" data-plugin="appear" data-animate="slide-bottom" data-repeat="false" m-type="nosysdata">
	                <button type="button" class="btn btn-primary btn-block btn-squared ladda-button" id="met-pager-btn" data-plugin="ladda" data-style="slide-left" data-url="" data-page="1">
	                    <i class="icon wb-chevron-down m-r-5" aria-hidden="true"></i>
	                </button>
	            </div>
            </if>
		</div>
	</div>
</section>