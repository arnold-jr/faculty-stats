site_template="http://virg.vanderbilt.edu/webtools/registry/FacDetail.aspx?fname=&lname=X&school=0&dept=0"
for k in {A..Z}
do url="http://virg.vanderbilt.edu/webtools/registry/FacDetail.aspx?fname=&lname=$k&school=0&dept=0"
curl $url -o "$k"_names.xml
done
