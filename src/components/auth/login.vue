<template>
  <div class="bg" flex="main:center cross:center dir:top">


    <div class="form" flex="main:center dir:top cross:center">

      <div class="item" flex="main:center cross:center box:first">
        <span>账号</span>
        <input type="text" placeholder="输入用户名">
      </div>
      <div class="item" flex="main:center cross:center box:first">
        <span>密码</span>
        <input type="password" placeholder="输入密码">
      </div>
      <a href="javascript:;" class="login " flex="main:center cross:center" @click="login">立即登录</a>
    </div>


  </div>
</template>

<script>
  export default {
    name: 'login',
    methods: {
      login(){
        localStorage.setItem('user', 'lishican')
        this.$router.replace({path: '/room'})
      }
    }
  }
</script>

<style scoped lang="scss">
  .bg{
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: #0c3f39;
    z-index: 200;
  }

  .form {
    width: 100%;

    .item {
      font-size: .32rem;
      color: #068a60;
      border: 1px solid #068a60;
      width: 80%;
      height: 1rem;
      border-radius: .95rem;
      box-shadow: 0 0 13px rgba(0, 0, 0, .1);
      span {
        margin: 0 .35rem;
      }
      input {
        font-size: .4rem;

      }
      margin-bottom: .4rem;
    }
    .login {
      display: inline-block;

      margin-top: 1rem;
      border: 1px solid #068a60;
      padding: .2rem .5rem;
      background: #31b655;
      text-decoration: none;
      border-radius: .5rem;
      color: #fff;
    }
    ::-webkit-input-placeholder {
      color: #999;
    }
  }
</style>
