from django.shortcuts import render, get_object_or_404, redirect
from django.utils import timezone
from django.views.generic import TemplateView, ListView, DetailView, CreateView, DeleteView, UpdateView
from blog import models
from django.contrib.auth.mixins import LoginRequiredMixin
from django.contrib.auth.decorators import login_required
from blog import forms
from django.urls import reverse_lazy
# Create your views here.

class AboutView(TemplateView):
    template_name = 'about.html'

class PostListView(ListView):
    model = models.Post

    def get_queryset(self):
        return models.Post.objects.filter(published_date__lte=timezone.now()).order_by('-published_date') # Documentation: Field Lookups

class PostDetailView(DetailView):
    model = models.Post

class PostCreateView(LoginRequiredMixin, CreateView):
    model = models.Post
    login_url = '/login/'
    redirect_field_name = 'blog/post_detail.html'
    form_class = forms.PostForm

class PostUpdateView(LoginRequiredMixin, UpdateView):
    model = models.Post
    login_url = '/login/'
    redirect_field_name = 'blog/post_detail.html'
    form_class = forms.PostForm

class PostDeleteView(LoginRequiredMixin, DeleteView):
    model = models.Post
    success_url = reverse_lazy('blog:post_list')

class DraftListView(LoginRequiredMixin, ListView):
    login_url = '/login/'
    redirect_field_name = 'blog/post_list.html'
    model = models.Post

    def get_queryset(self):
        return models.Post.objects.filter(published_date__isnull=True).order_by('create_date')

######################################
@login_required
def post_publish(request, pk):
    post = get_object_or_404(models.Post, pk=pk)
    post.publish()
    return redirect('blog:post_detail', pk=post.pk)


@login_required
def add_comment_to_post(request, pk):
    post = get_object_or_404(models.Post, pk=pk)
    if request.method == 'POST':
        form = forms.CommentForm(request.POST)
        if form.is_valid():
            comment = form.save(commit=False)
            comment.post = post
            comment.save()
            return redirect('blog:post_detail', pk=post.pk)
    else:
        form = forms.CommentForm()
    return render(request, 'blog/comment_form.html', {'form':form})

@login_required
def comment_approve(request, pk):
    comment = get_object_or_404(models.Comment, pk=pk)
    comment.approve()
    return redirect('blog:post_detail', pk=comment.post.pk)

@login_required
def delete_comment(request, pk):
    comment = get_object_or_404(models.Comment, pk=pk)
    post_pk = comment.post.pk # tady se musí vytvořit variable pro comment.post.pk 
    comment.delete()
    return redirect('blog:post_detail', pk=post_pk) # až se odstraní, systém si ho nebude pamatovat