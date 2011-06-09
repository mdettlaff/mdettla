using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using nReddit.Models;
using System.Data;

namespace nReddit.Controllers
{
    public class CommentController : Controller
    {
        private NRedditEntities db = new NRedditEntities();

        //
        // GET: /Comment/Create

        [Authorize]
        public ActionResult Create()
        {
            return View();
        }

        //
        // POST: /Comment/Create

        [HttpPost]
        [Authorize]
        public ActionResult Create(Comment comment)
        {
            if (ModelState.IsValid)
            {
                int submissionID = (int)Session["submissionID"];
                comment.Username = User.Identity.Name;
                Submission submission =
                    db.Submissions.Include("Comments").Single(s => s.SubmissionID == submissionID);
                db.Comments.Add(comment);
                submission.Comments.Add(comment);
                db.Entry(submission).State = EntityState.Modified;
                db.SaveChanges();
                return RedirectToAction("Details", "Submission", new { id = submissionID });
            }
            return View(comment);
        }

        //
        // GET: /Comment/Delete/5

        [Authorize]
        public ActionResult Delete(int id)
        {
            int submissionID = (int)Session["submissionID"];
            ViewBag.SubmissionID = submissionID;
            Comment comment = db.Comments.Find(id);
            if (!authorizeCreator(comment))
            {
                return RedirectToAction("Error", new
                {
                    id = submissionID,
                    message = "Można usuwać tylko własne komentarze"
                });
            }
            return View(comment);
        }

        //
        // POST: /Comment/Delete/5

        [Authorize]
        [HttpPost, ActionName("Delete")]
        public ActionResult DeleteConfirmed(int id)
        {
            int submissionID = (int)Session["submissionID"];
            Comment comment = db.Comments.Find(id);
            if (!authorizeCreator(comment))
            {
                return RedirectToAction("Error", new
                {
                    id = submissionID,
                    message = "Można usuwać tylko własne komentarze"
                });
            }
            db.Comments.Remove(comment);
            db.SaveChanges();
            return RedirectToAction("Details", "Submission", new { id = submissionID });
        }

        public ActionResult Error(int id, string message)
        {
            ViewBag.SubmissionID = id;
            ViewBag.Message = message;
            return View();
        }

        private bool authorizeCreator(Comment comment)
        {
            return User.Identity.Name.Equals(comment.Username) || User.IsInRole("Administrator");
        }
    }       
}
