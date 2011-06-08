using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel;

namespace nReddit.Models
{
    public class Comment
    {
        public int CommentID { get; set; }
        [Required(ErrorMessage = "Podaj treść komentarza")]
        [DisplayName("Treść")]
        [StringLength(128)]
        public string Content { get; set; }
        [ScaffoldColumn(false)]
        public int UpvoteCount { get; set; }
        [ScaffoldColumn(false)]
        public int DownvoteCount { get; set; }
        [ScaffoldColumn(false)]
        public string Username { get; set; }

        [ScaffoldColumn(false)]
        public int Score
        {
            get
            {
                return UpvoteCount - DownvoteCount;
            }
        }

        public void Upvote()
        {
            UpvoteCount++;
        }

        public void Downvote()
        {
            DownvoteCount++;
        }

    }
}