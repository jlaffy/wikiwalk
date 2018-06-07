library(dplyr)
# library(reshape)
library(reshape2)
library(httr)

# example of wiki API request
# https://en.wikipedia.org/w/api.php?action=query&titles= Circadian%20rhythm &prop=revisions&rvprop=ids&rvstart=01012001&rvdir=newer&format=json&rvcontinue=1
get_article_full_history_table=function(article_name){
    what="ids|flags|timestamp|comment|user|userid|size|content|parsedcomment|tags"
  #article_name="Circadian rhythm"
  article_name_c=gsub(" ","%20",article_name)
    output_table=c()
    cmd=paste("https://en.wikipedia.org/w/api.php?action=query&titles=",article_name_c,"&prop=revisions&rvprop=",what,"&rvstart=01012001&rvdir=newer&format=json&rvlimit=max",sep="")
	  resp=GET(cmd)
	  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
	    tt=paste("parsed$query$pages$'",names(parsed$query$pages)[1],"'$revisions",sep="")
	    name_tab=rep(article_name,dim(eval(parse(text=tt)))[1])
		  output_table=cbind(art=name_tab,eval(parse(text=tt)))


		  while(length(parsed$continue$rvcontinue)==1){
			    output_table_load=c()
		      print(parsed$continue$rvcontinue)
			      rvc=parsed$continue$rvcontinue
			      cmd=paste("https://en.wikipedia.org/w/api.php?action=query&titles=",article_name_c,"&prop=revisions&rvprop=",what,"&rvstart=01012001&rvdir=newer&format=json&rvlimit=max&rvcontinue=",rvc,sep="")
				      resp=GET(cmd)
				      parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = T)
					      tt2=paste("parsed$query$pages$'",names(parsed$query$pages)[1],"'$revisions",sep="")
					      name_tab2=rep(article_name,dim(eval(parse(text=tt2)))[1])
						      print(length(name_tab2))
						      if(length(name_tab2)<1) break
							      output_table_load=cbind(art=name_tab2,eval(parse(text=tt2)))
							      print(dim(output_table))
								      print(dim(output_table_load))
								      print(colnames(output_table_load))

									        output_table=try(rbind(output_table,output_table_load[,1:15]),silent=T)
									    }
		    return(output_table)
}

test_wiki_hist=get_article_full_history_table("Circadian rhythm")
