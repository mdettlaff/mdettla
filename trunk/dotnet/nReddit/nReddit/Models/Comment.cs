using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace nReddit.Models
{
    public class Comment
    {
        public int CommentID { get; set; }
        public string Content { get; set; }
        public int UpvoteCount { get; set; }
        public int DownvoteCount { get; set; }

        public int Score
        {
            get
            {
                return UpvoteCount - DownvoteCount;
            }
        }
    }
}