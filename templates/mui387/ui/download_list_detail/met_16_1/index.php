<?php defined('IN_MET') or exit('No permission'); ?>
<main class="$uicss met-download animsition" m-id='{$ui.mid}'>
    <div class="container">
        <div class="row">
        <if value="$ui[has][sidebar]">
        	<div class="met-download-list col-md-9">
        	<else/>
        	<div class="col-md-10 offset-md-1" m-id='noset'>
        </if>
				<div class="row">
					<section class="details-title border-bottom1">
						<h1 class='m-t-10 m-b-5'>{$data.title}</h1>
						<div class="info">
							<span class="p-r-10">{$data.updatetime}</span>
							<span class="p-r-10">{$data.issue}</span>
							<span class="p-r-10">
								<i class="icon wb-eye m-r-5" aria-hidden="true"></i>
								{$data.hits}
							</span>
						</div>
					</section>
					<section class="download-paralist p-y-20 border-bottom1">
						<list data="$data['para']" name='$s'>
							<if value="$s">
								<if value="$ui['is_view_para']">
									<dl class="dl-horizontal font-size-16">
										<dt class='inline-block font-weight-300'>{$s.name} ：</dt>
										<dd class="inline-block blue-grey-500">{$s.value}</dd>
									</dl>
								<else/>
									<?php 
										$img_info = $s['name'];
	 									$domain = strstr($img_info, 'image');
									?>
									<if value="$domain eq null">
										<dl class="dl-horizontal font-size-16">
											<dt class='inline-block font-weight-300'>{$s.name} ：</dt>
											<dd class="inline-block blue-grey-500">{$s.value}</dd>
										</dl>
									<else/>
										
									</if>
								</if>
								
							</if>
						</list>
						<a class="btn btn-outline btn-primary btn-squared met-download-btn" href="{$data.downloadurl}" title="{$data.title}">{$ui.download}</a>
					</section>
					<section class="met-editor clearfix">
						{$data.content}
					</section>
					<if value="$ui['tag_ok']">
	                    <div class="tag">
	                        <span>{$data.tagname}</span>
	                        <list data="$data[taglist]" name="$tag" num="$ui[tag_num]">
	                            <a href="{$tag.url}" title="{$tag.name}">{$tag.name}</a>
	                        </list>
	                    </div>
	                </if>
	                <pagination/>
				</div>
				</div>
<if value="!$ui[has][sidebar]">
		</div>
	</div>
</main>
</if>