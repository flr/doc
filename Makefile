all:
	R -e "rmarkdown::render_site(output_format='html_document')"
